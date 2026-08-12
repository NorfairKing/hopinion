{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The fact store: what one package's phase leaves behind for the phases that
-- can see further.
--
-- A database rather than a file of records, because what the later phases do
-- with facts is join them. One table per fact, owned by the rule that writes
-- it, so a rule brings its own schema the way it brings its own check.
--
-- A fact is a row rather than a keyed entry: two instances in one module are
-- two facts, and there is nothing to key them on that is not invented. Row
-- identifiers mean nothing outside the store that chose them, so 'mergeStore'
-- leaves them behind and each fact arrives as a new row.
module Hopinion.Store
  ( Carry,
    Query,
    StoreLocation (..),
    StoredPath (..),
    withStore,
    withMemoryStore,
    mergeStore,
    StoredPackage (..),
    StoredModule (..),
    StoredExpectedModule (..),
    StoredAnnotation (..),
    StoredMeta (..),
    -- | The envelope's columns, so a rule can join what it wrote against what
    -- it can rely on, and its keys, so a caller can write a row into a table
    -- the envelope keys itself.
    EntityField (..),
    Key (..),
    envelopeMigration,
    writePackageEnvelope,
    writeModuleEnvelope,
    packageNames,
    genPackageFor,
    storedModules,
    storedAnnotations,
    annotationsOfPackage,
    writeMeta,
    expectedModulesOf,
    writtenByAnotherVersion,
    storedFormatOf,
  )
where

import Conduit (runConduit, (.|))
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.Conduit.List as CL
import Data.Functor.Identity (Identity (..))
import Data.Text (Text)
import qualified Data.Text as T
import Database.Esqueleto.Experimental
import Database.Persist.Sqlite (SqliteConnectionInfo, mkSqliteConnectionInfo, walEnabled, withSqliteConnInfo)
import Database.Persist.TH
import Hopinion.Facts
import Path (Abs, File, Path, Rel, parseRelFile, toFilePath)

-- | Writing to the store, and reading from it. Named so that a rule's signature
-- says which of the two it is doing without naming a backend.
type Carry = SqlPersistT (NoLoggingT (ResourceT IO)) ()

type Query a = SqlPersistT (NoLoggingT (ResourceT IO)) a

-- | A repository-relative path at the database boundary, and nowhere else.
--
-- The domain carries @Path Rel File@, which needs no wrapper of its own: it
-- already cannot be absolute or a directory. persistent is what needs a name,
-- because the quasi-quoter takes one per column and this package has to own the
-- 'PersistField' instance behind it, which for @Path Rel File@ would be an
-- orphan.
--
-- Stored as the text it renders to and read back through the same parser every
-- other path goes through, so a row that is not a relative file path fails here
-- rather than inside a rule.
newtype StoredPath = StoredPath {storedPathFile :: Path Rel File}
  deriving (Show, Eq)

instance PersistField StoredPath where
  toPersistValue = toPersistValue . relPathText . storedPathFile
  fromPersistValue v = do
    t <- fromPersistValue v
    maybe
      (Left "not a repository-relative file path")
      (Right . StoredPath)
      (parseRelFile (T.unpack t))

instance PersistFieldSql StoredPath where
  sqlType _ = SqlString

-- | The envelope's own tables: what every rule can rely on and no rule has to
-- write.
--
-- The indexes are keyed on what they are about and written with a plain insert
-- rather than an upsert, so a second row for one key is a constraint violation
-- rather than a silent overwrite. It is a violation and not a report: writing
-- one is a mistake in whatever enumerated the packages or the modules, and the
-- caller has to have ruled it out before it gets here. Discovery does rule out
-- two packages by one name, which is what 'Hopinion.Project.discoverPackages'
-- refuses as @TwoPackagesCalled@. The stamp is the exception, and 'writeMeta'
-- says why.
--
-- Suppressions are rows, since a module has as many as it has, and one opaque
-- column each, since nothing queries into one.
share
  [mkPersist sqlSettings, mkMigrate "envelopeMigration"]
  [persistLowerCase|
StoredMeta
    formatVersion Word
    Primary formatVersion
    deriving Show Eq

StoredPackage
    name PackageName
    role PackageRole
    cabal StoredPath
    Primary name
    deriving Show Eq

StoredModule
    package PackageName
    moduleRef ModuleRef
    path StoredPath
    outcome ParseOutcome
    Primary package moduleRef
    deriving Show Eq

StoredExpectedModule
    package PackageName
    moduleRef ModuleRef
    Primary package moduleRef
    deriving Show Eq

StoredAnnotation
    package PackageName
    moduleRef ModuleRef
    annotation AnnotationFact
    deriving Show Eq
|]

-- | Where a store lives.
--
-- Two constructors rather than a path, because SQLite's word for having no file
-- at all is a file name that is not one, and a magic string is a state a type
-- can rule out.
data StoreLocation
  = InMemory
  | OnDisk !(Path Abs File)
  deriving (Show, Eq)

storeLocationText :: StoreLocation -> Text
storeLocationText = \case
  InMemory -> ":memory:"
  OnDisk p -> T.pack (toFilePath p)

-- | A store, migrated for the envelope and for every rule that brought a
-- schema.
--
-- Without WAL: a store written with it stays in WAL mode, and reading one
-- creates files beside it, which the Nix store does not allow.
withStore :: [Migration] -> StoreLocation -> Query a -> IO a
withStore migrations location act =
  runResourceT $
    runNoLoggingT $
      withSqliteConnInfo (withoutWal (mkSqliteConnectionInfo (storeLocationText location))) $
        runSqlConn $ do
          mapM_ runMigrationQuiet (envelopeMigration : migrations)
          act

-- | The same store with nowhere to put it, which is what the one-process run
-- uses, and why the two paths can be asserted to agree.
withMemoryStore :: [Migration] -> Query a -> IO a
withMemoryStore migrations = withStore migrations InMemory

-- | Copy everything out of another store into this one, or say which of its
-- tables this one has nowhere to put.
--
-- Every table, found by asking the other database what it has, so a rule that
-- brings a new one is merged without this knowing it exists. Read over a second
-- connection rather than through ATTACH, which SQLite refuses inside the
-- transaction persistent is always in.
--
-- Checked before a row is copied rather than discovered on one, or an
-- executable with a rule this one lacks fails inside SQLite naming something
-- nobody chose.
mergeStore :: Path Abs File -> Query [Text]
mergeStore path = do
  incoming <- liftIO (readEverything path)
  here <- tableNames
  case [incomingTableName t | t <- incoming, incomingTableName t `notElem` here] of
    [] -> do
      mapM_ insertAll incoming
      pure []
    missing ->
      pure
        [ T.concat
            [ "The facts in ",
              T.pack (toFilePath path),
              " hold ",
              T.intercalate ", " missing,
              ", which this build of hopinion has no rule for and so no table for.",
              " The package outputs and this run were made by different executables."
            ]
        ]
  where
    insertAll :: IncomingTable -> Carry
    insertAll t = mapM_ (insertRow t) (incomingTableRows t)

    -- Insert or ignore, because the one row every store holds is the format
    -- stamp, and two stores at this format hold the same one.
    insertRow :: IncomingTable -> [PersistValue] -> Carry
    insertRow t row =
      rawExecute
        ( T.concat
            [ "INSERT OR IGNORE INTO ",
              quoted (incomingTableName t),
              " (",
              T.intercalate "," (map quoted (incomingTableColumns t)),
              ") VALUES (",
              T.intercalate "," (replicate (length row) "?"),
              ")"
            ]
        )
        row

-- | One table of another store, named column by column so that the row a fact
-- arrives as is not the row it is written as.
data IncomingTable = IncomingTable
  { incomingTableName :: !Text,
    incomingTableColumns :: ![Text],
    incomingTableRows :: ![[PersistValue]]
  }

-- | Everything in another store, minus the row identifiers, which mean nothing
-- outside the store that chose them: copying them would collide two packages'
-- facts on numbers neither picked.
readEverything :: Path Abs File -> IO [IncomingTable]
readEverything path =
  runResourceT $
    runNoLoggingT $
      withSqliteConnInfo (withoutWal (mkSqliteConnectionInfo (storeLocationText (OnDisk path)))) $
        runReaderT $ do
          names <- rawSql "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%' ORDER BY name" []
          traverse (\(Single t) -> tableOf t) names

-- | Every table this store has, which is what an incoming one is measured
-- against.
tableNames :: Query [Text]
tableNames =
  map unSingle
    <$> rawSql "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%'" []

-- | The format a store says it was written in, or nothing when it does not say.
--
-- Asked before anything is merged: a merge copies column by column, so a store
-- from another format would answer with a SQLite error about a column rather
-- than the sentence the stamp exists to produce.
storedFormatOf :: Path Abs File -> IO (Maybe Word)
storedFormatOf path =
  runResourceT $
    runNoLoggingT $
      withSqliteConnInfo (withoutWal (mkSqliteConnectionInfo (storeLocationText (OnDisk path)))) $
        runReaderT $ do
          stamped <-
            rawSql
              "SELECT name FROM sqlite_master WHERE type = 'table' AND name = 'stored_meta'"
              []
          case (stamped :: [Single Text]) of
            [] -> pure Nothing
            _ -> do
              versions <- rawSql "SELECT format_version FROM stored_meta ORDER BY format_version LIMIT 1" []
              pure (case versions of Single v : _ -> Just v; [] -> Nothing)

tableOf :: Text -> Query IncomingTable
tableOf t = do
  columns <- filter (/= "id") <$> columnsOf t
  rows <- rowsOf t columns
  pure IncomingTable {incomingTableName = t, incomingTableColumns = columns, incomingTableRows = rows}

-- | What a table's columns are called, asked of the database rather than known,
-- so a rule bringing a new table is merged without this learning it exists.
--
-- A table with no columns is a failure rather than an empty answer: it would be
-- a table quietly not merged, and a rule's facts quietly missing is a finding
-- against code that is fine.
columnsOf :: Text -> Query [Text]
columnsOf t = do
  rows <- runConduit (rawQuery (T.concat ["PRAGMA table_info(", quoted t, ")"]) [] .| CL.consume)
  let named = [name | (_ : PersistText name : _) <- rows]
  if length named == length rows && not (null named)
    then pure named
    else
      liftIO
        ( throwIO
            ( userError
                ( unwords
                    [ "Could not read the columns of",
                      T.unpack t,
                      "in a store being merged, so its rows would have been dropped in silence."
                    ]
                )
            )
        )

-- | Every row of a table, over the columns 'columnsOf' answered with, which is
-- never none of them: it fails rather than answering with an empty list.
rowsOf :: Text -> [Text] -> Query [[PersistValue]]
rowsOf t columns =
  runConduit
    ( rawQuery
        (T.concat ["SELECT ", T.intercalate "," (map quoted columns), " FROM ", quoted t])
        []
        .| CL.consume
    )

-- | A connection that only reads. Left to itself it sets journal_mode=WAL,
-- which is a write, and a package's store under Nix is read only. Outside a
-- transaction for the same reason: opening one is a write.
withoutWal :: SqliteConnectionInfo -> SqliteConnectionInfo
withoutWal = runIdentity . walEnabled (const (Identity False))

quoted :: Text -> Text
quoted t = T.concat ["\"", T.replace "\"" "\"\"" t, "\""]

writePackageEnvelope ::
  PackageName ->
  PackageRole ->
  Path Rel File ->
  [ModuleRef] ->
  Carry
writePackageEnvelope name role cabal expected = do
  insertKey
    (StoredPackageKey name)
    StoredPackage
      { storedPackageName = name,
        storedPackageRole = role,
        storedPackageCabal = StoredPath cabal
      }
  mapM_
    ( \ref ->
        insertKey
          (StoredExpectedModuleKey name ref)
          StoredExpectedModule
            { storedExpectedModulePackage = name,
              storedExpectedModuleModuleRef = ref
            }
    )
    expected

writeModuleEnvelope ::
  PackageName ->
  ModuleRef ->
  Path Rel File ->
  ParseOutcome ->
  [AnnotationFact] ->
  Carry
writeModuleEnvelope pkg ref path outcome annotations = do
  insertKey
    (StoredModuleKey pkg ref)
    StoredModule
      { storedModulePackage = pkg,
        storedModuleModuleRef = ref,
        storedModulePath = StoredPath path,
        storedModuleOutcome = outcome
      }
  mapM_
    ( \a ->
        insert_
          StoredAnnotation
            { storedAnnotationPackage = pkg,
              storedAnnotationModuleRef = ref,
              storedAnnotationAnnotation = a
            }
    )
    annotations

packageNames :: Query [PackageName]
packageNames =
  map unValue
    <$> select
      ( do
          p <- from (table @StoredPackage)
          orderBy [asc (p ^. StoredPackageName)]
          pure (p ^. StoredPackageName)
      )

-- | Where a test satisfying an obligation from this package has to live.
--
-- A gen package tests itself; a main package is tested by the gen package named
-- after it. That convention is the whole of the pairing, so this asks after one
-- package rather than scanning a table of every pair.
genPackageFor :: PackageName -> Query GenPackage
genPackageFor pkg = do
  role <-
    fmap unValue
      <$> selectOne
        ( do
            p <- from (table @StoredPackage)
            where_ (p ^. StoredPackageName ==. val pkg)
            pure (p ^. StoredPackageRole)
        )
  case role of
    Just RoleGen -> pure (GenPackage pkg)
    Just RoleMain -> generatorPackageNamed (genNameOf pkg)
    -- A package the store never heard of has no facts, so it has made no
    -- obligation that needs a home. The name is still the name it would have.
    Nothing -> pure (NoGenPackage (genNameOf pkg))

genNameOf :: PackageName -> PackageName
genNameOf pkg = PackageName (T.concat [packageNameText pkg, "-gen"])

generatorPackageNamed :: PackageName -> Query GenPackage
generatorPackageNamed gen =
  maybe (NoGenPackage gen) (GenPackage . unValue)
    <$> selectOne
      ( do
          p <- from (table @StoredPackage)
          where_ (p ^. StoredPackageName ==. val gen)
          pure (p ^. StoredPackageName)
      )

-- | Every module the store knows about, which is what tells the project phase
-- both what to look for a build's output for and which modules could not be
-- read.
storedModules :: Query [StoredModule]
storedModules =
  map entityVal
    <$> select
      ( do
          m <- from (table @StoredModule)
          orderBy [asc (m ^. StoredModulePackage), asc (m ^. StoredModuleModuleRef)]
          pure m
      )

-- | Every suppression in the store, which is what the project phase judges.
storedAnnotations :: Query [AnnotationFact]
storedAnnotations =
  map unValue
    <$> select
      ( do
          a <- from (table @StoredAnnotation)
          orderBy
            [ asc (a ^. StoredAnnotationPackage),
              asc (a ^. StoredAnnotationModuleRef),
              asc (a ^. StoredAnnotationId)
            ]
          pure (a ^. StoredAnnotationAnnotation)
      )

-- | Only this package's suppressions, which is what the package phase judges.
annotationsOfPackage :: PackageName -> Query [AnnotationFact]
annotationsOfPackage pkg =
  map unValue
    <$> select
      ( do
          a <- from (table @StoredAnnotation)
          where_ (a ^. StoredAnnotationPackage ==. val pkg)
          orderBy [asc (a ^. StoredAnnotationModuleRef), asc (a ^. StoredAnnotationId)]
          pure (a ^. StoredAnnotationAnnotation)
      )

-- | What a package's cabal file says its components hold, by component, since
-- @Main@ is declared by every one that has a @main-is@ and covering one of them
-- is not covering the others.
expectedModulesOf :: PackageName -> Query [ModuleRef]
expectedModulesOf name =
  map unValue
    <$> select
      ( do
          e <- from (table @StoredExpectedModule)
          where_ (e ^. StoredExpectedModulePackage ==. val name)
          orderBy [asc (e ^. StoredExpectedModuleModuleRef)]
          pure (e ^. StoredExpectedModuleModuleRef)
      )

-- | Whether anything in here was written in a format that is not this one's.
--
-- A merged store holds one row per format it was written in, since that is what
-- the stamp is keyed on, so a foreign store shows up as a second row.
writtenByAnotherVersion :: Query Bool
writtenByAnotherVersion =
  not . null
    <$> select
      ( do
          m <- from (table @StoredMeta)
          where_ (m ^. StoredMetaFormatVersion !=. val (formatVersionNumber currentFormatVersion))
          limit 1
          pure (m ^. StoredMetaFormatVersion)
      )

-- | An upsert, unlike everything else the envelope writes: the project phase
-- stamps its own store and then merges stores that each carry one, so writing
-- the same stamp twice is that path working.
writeMeta :: Carry
writeMeta =
  repsert
    (StoredMetaKey (formatVersionNumber currentFormatVersion))
    StoredMeta {storedMetaFormatVersion = formatVersionNumber currentFormatVersion}
