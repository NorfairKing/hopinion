{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Discovery: which packages a repository has, what is in them, and how to read
-- one of their files.
--
-- Packages are found by walking for @.cabal@ files rather than by reading
-- @stack.yaml@, because the cabal file is what the build reads and walking
-- needs no stack knowledge.
module Hopinion.Project
  ( SourceRoot (..),
    relPathIn,
    sourceFileIn,
    PackageModel (..),
    ComponentModel (..),
    ModuleModel (..),
    ModuleSource (..),
    DiscoveryError (..),
    renderDiscoveryError,
    discoverPackages,
    skippedByName,
    readPackageModel,
    unclaimedSourceFiles,
    readSource,
  )
where

import Control.Monad (filterM)
import qualified Data.ByteString as BS
import Data.List (intercalate, isPrefixOf, sort, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, isJust)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Distribution.ModuleName as CabalModule
import Distribution.PackageDescription
  ( BuildInfo (..),
    Executable (..),
    PackageDescription (..),
    TestSuite (..),
  )
import qualified Distribution.PackageDescription as Cabal
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Types.Library (Library (..))
import Distribution.Types.LibraryName (LibraryName (..))
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Utils.Path (getSymbolicPath)
import Hopinion.Facts
import qualified Language.Haskell.Extension as CabalExt
import Path
  ( Abs,
    Dir,
    File,
    Path,
    Rel,
    dirname,
    fileExtension,
    parent,
    parseRelDir,
    parseRelFile,
    reldir,
    stripProperPrefix,
    toFilePath,
    (</>),
  )
import Path.IO (forgivingAbsence, getModificationTime, listDir)
import qualified System.FilePath as FP

-- | Where the sources are, and what every reported path is relative to. The
-- two differ under Nix, where a package derivation is given only its own
-- subtree and its paths still have to read as repository-relative.
--
-- 'Nothing' for the prefix rather than an empty one, because a repository is
-- what reported paths are already relative to and joining in a @.@ would
-- produce paths like @hopinion/.@.
data SourceRoot = SourceRoot
  { sourceRootDir :: !(Path Abs Dir),
    sourceRootPrefix :: !(Maybe (Path Rel Dir))
  }
  deriving (Show, Eq)

-- | What to call a file the tool read, in a report or a fact.
--
-- 'Nothing' when the file is not under this root at all, which is a caller
-- mistake rather than a fact about the code.
relPathIn :: SourceRoot -> Path Abs File -> Maybe (Path Rel File)
relPathIn root f = do
  inside <- stripProperPrefix (sourceRootDir root) f
  pure (maybe inside (</> inside) (sourceRootPrefix root))

-- | The inverse of 'relPathIn': where to read the file a reported path names.
--
-- 'Nothing' when the path is outside this root, which is the ordinary case for
-- a run given one root per package: each path belongs to exactly one of them.
sourceFileIn :: SourceRoot -> Path Rel File -> Maybe (Path Abs File)
sourceFileIn root rp =
  case sourceRootPrefix root of
    Nothing -> Just (sourceRootDir root </> rp)
    Just prefix -> (sourceRootDir root </>) <$> stripProperPrefix prefix rp

data ModuleModel = ModuleModel
  { moduleModelKey :: !ModuleKey,
    -- | Absolute, for reading.
    moduleModelFile :: !(Path Abs File),
    -- | Repo-relative, for every fact and finding.
    moduleModelRelPath :: !(Path Rel File),
    moduleModelSource :: !ModuleSource
  }
  deriving (Show, Eq)

-- | Whether the file behind a declared module is Haskell to read or a
-- preprocessor input, which holds no Haskell and so is read by nothing here.
data ModuleSource
  = HaskellSource
  | PreprocessedSource
  deriving (Show, Eq)

data ComponentModel = ComponentModel
  { componentModelKind :: !ComponentKind,
    -- | What the cabal file calls this component, which tells two of them
    -- apart when their kinds and their module names are the same: a package
    -- with four test suites has four modules called @Main@.
    componentModelName :: !ComponentName,
    componentModelDefaultExtensions :: ![Text],
    componentModelModules :: ![ModuleModel],
    -- | What the cabal file says the component contains, which the project
    -- layer asserts the facts cover. From the cabal file rather than from what
    -- resolved, or the assertion would compare a list against itself and a
    -- module that vanished from disk would go unnoticed.
    componentModelDeclaredModules :: ![ModuleKey],
    componentModelSourceDirs :: ![Path Abs Dir]
  }
  deriving (Show, Eq)

data PackageModel = PackageModel
  { packageModelName :: !PackageName,
    packageModelRole :: !PackageRole,
    packageModelDir :: !(Path Abs Dir),
    -- | The cabal file, which is a real file to point a reader at where the
    -- directory it sits in is not.
    packageModelCabal :: !(Path Rel File),
    packageModelComponents :: ![ComponentModel],
    -- | What the cabal file declares as extra source files. That is data
    -- rather than modules, so a Haskell file there is not a module that fell
    -- out of a component.
    packageModelDataPaths :: ![Path Abs Dir]
  }
  deriving (Show, Eq)

-- | What can stop a repository from being read at all, as what happened rather
-- than as the sentence about it.
--
-- Typed, so that a new way of failing is a constructor every site has to
-- reconsider rather than a string somebody has to go and find, and so that the
-- sentence a reader sees is written in one place. Cabal's own parse errors are
-- the exception: they are that library's rendering of its own syntax, passed
-- through rather than owned here.
data DiscoveryError
  = NoDirectoryAt !(Path Abs Dir)
  | NoCabalFileUnder !(Path Abs Dir)
  | CabalFileOutsideRoot !(Path Abs File)
  | CabalFileUnparseable !(Path Abs File) !Text
  | TwoPackagesCalled !PackageName !(NonEmpty (Path Rel File))
  deriving (Show, Eq)

renderDiscoveryError :: DiscoveryError -> Text
renderDiscoveryError = \case
  NoDirectoryAt dir ->
    T.pack (unwords ["There is no directory at", toFilePath dir, "to read packages from."])
  NoCabalFileUnder dir ->
    T.pack (unwords ["No cabal file under", toFilePath dir])
  CabalFileOutsideRoot cabalFile ->
    T.pack (unwords ["Outside the source root:", toFilePath cabalFile])
  CabalFileUnparseable cabalFile errs ->
    T.pack (unwords ["Failed to parse", toFilePath cabalFile, ":", T.unpack errs])
  TwoPackagesCalled name cabalFiles ->
    T.pack
      ( unwords
          ( [ "Two packages are called",
              T.unpack (packageNameText name),
              "and one name is all the facts of either can be filed under:"
            ]
              ++ [T.unpack (relPathText c) | c <- NE.toList cabalFiles]
          )
      )

-- | Every directory holding a @.cabal@ file, in name order so that a run is
-- reproducible.
discoverPackages :: SourceRoot -> Path Abs Dir -> IO (Either DiscoveryError [PackageModel])
discoverPackages root from = do
  -- The walk itself says whether the root is there, rather than a question
  -- asked first: a producing command that dies has written no report, and
  -- anything between the asking and the walking could delete the directory.
  walked <- forgivingAbsence (packageCabalFiles from)
  case walked of
    Nothing -> pure (Left (NoDirectoryAt from))
    Just cabalFiles -> do
      models <- traverse (readPackageModel root) cabalFiles
      pure (sequence models >>= assertNamesAreUnique)
  where
    -- A package name is what every fact is filed under, so two cabal files
    -- claiming one name would file both packages' facts as one package's. Said
    -- here, as a complaint about the repository, because the alternatives are a
    -- fact quietly overwriting another and a constraint violation from inside
    -- the store, neither of which a person can act on.
    assertNamesAreUnique :: [PackageModel] -> Either DiscoveryError [PackageModel]
    assertNamesAreUnique models =
      case [group | group@(_ :| (_ : _)) <- byName models] of
        [] -> Right models
        (clashing@(first :| _) : _) ->
          Left
            ( TwoPackagesCalled
                (packageModelName first)
                (NE.map packageModelCabal clashing)
            )

    -- NonEmpty because a group of equals has at least the one it is a group of,
    -- which lets the message name the package without a partial function.
    byName :: [PackageModel] -> [NonEmpty PackageModel]
    byName models =
      NE.groupBy
        (\x y -> packageModelName x == packageModelName y)
        (sortOn packageModelName models)

-- | The cabal file of every package under a directory, in name order so that a
-- run is reproducible. A directory with more than one is read through the first,
-- which is what cabal itself would refuse to do and nothing here has to decide.
packageCabalFiles :: Path Abs Dir -> IO [Path Abs File]
packageCabalFiles = go
  where
    -- Descending stops at a package: cabal packages do not nest, so anything
    -- below one is that package's own material. Otherwise a package whose test
    -- resources are little repositories would have them discovered as packages
    -- of the repository under analysis.
    go dir = do
      (subs, files) <- listDir dir
      case sort (filter isCabalFile files) of
        (cabalFile : _) -> pure [cabalFile]
        [] -> concat <$> traverse go (sort (filter (not . skippedByName . dirname) subs))

    isCabalFile :: Path Abs File -> Bool
    isCabalFile f = fileExtension f == Just ".cabal"

-- | The directories the walk does not go into, knowing only their name.
--
-- None of this is needed inside a Nix build, where the source is a store path
-- copied from the git tree and an ignored directory never arrives. It is for a
-- working tree, which the development loop is pointed at, and there they are
-- all present.
--
-- @dist-newstyle@ earns its place: cabal unpacks a @source-repository-package@
-- into @dist-newstyle\/src@, cabal file and all, so a walk that goes in there
-- reports a dependency's code as this repository's. A hidden directory is the
-- same story, @.stack-work@ most of all.
--
-- Whole names, not prefixes: a prefix takes @results@ along with the @result@
-- it means, and the packages under it go missing with nothing said, because a
-- walk that finds no cabal file and one that refused to look are the same
-- answer to a caller.
--
-- Neither @result@ nor @node_modules@ is named here. @result@ is a symlink to a
-- build output that holds no cabal file, and refusing to follow a symlink would
-- lose a package a repository links in on purpose. Nothing puts a Haskell
-- package under @node_modules@, so skipping it only adds a name this could be
-- wrong about.
skippedByName :: Path Rel Dir -> Bool
skippedByName d =
  "." `isPrefixOf` name
    || d == [reldir|dist-newstyle|]
  where
    name :: String
    name = toFilePath d

readPackageModel :: SourceRoot -> Path Abs File -> IO (Either DiscoveryError PackageModel)
readPackageModel root cabalFile = do
  contents <- BS.readFile (toFilePath cabalFile)
  case (snd (runParseResult (parseGenericPackageDescription contents)), relPathIn root cabalFile) of
    (_, Nothing) ->
      pure (Left (CabalFileOutsideRoot cabalFile))
    (Left (_, errs), _) ->
      pure (Left (CabalFileUnparseable cabalFile (T.pack (show errs))))
    (Right gpd, Just relCabal) -> do
      let pd = flattenPackageDescription gpd
      let dir = parent cabalFile
      let name = T.pack (Cabal.unPackageName (Cabal.pkgName (package pd)))
      components <- traverse (componentOf root dir) (componentsOf pd)
      pure
        ( Right
            PackageModel
              { packageModelName = PackageName name,
                packageModelRole = if T.isSuffixOf "-gen" name then RoleGen else RoleMain,
                packageModelDir = dir,
                packageModelCabal = relCabal,
                packageModelComponents = components,
                packageModelDataPaths = map (dir </>) (dataPathsOf pd)
              }
        )

-- | What every component declares, before any of it is resolved to files on
-- disk. @main-is@ is a file path rather than a module name, so it is carried
-- separately: the module inside it is @Main@ whatever the file is called, and
-- looking for @Main.hs@ instead would miss a suite whose main is @Spec.hs@.
componentsOf :: PackageDescription -> [DeclaredComponent]
componentsOf pd =
  concat
    [ [ DeclaredComponent ComponentLib (nameOfLibrary l) (libBuildInfo l) (libraryModules l) Nothing
      | Just l <- [library pd]
      ],
      [ DeclaredComponent ComponentLib (nameOfLibrary l) (libBuildInfo l) (libraryModules l) Nothing
      | l <- subLibraries pd
      ],
      [ DeclaredComponent
          ComponentApp
          (unqualName (exeName e))
          (buildInfo e)
          (otherModules (buildInfo e))
          (parseRelFile (modulePath e))
      | e <- executables pd
      ],
      [ DeclaredComponent
          ComponentTest
          (unqualName (testName t))
          (testBuildInfo t)
          (otherModules (testBuildInfo t))
          (mainOfTest t)
      | t <- testSuites pd
      ],
      [ DeclaredComponent
          ComponentBench
          (unqualName (Cabal.benchmarkName b))
          (Cabal.benchmarkBuildInfo b)
          (otherModules (Cabal.benchmarkBuildInfo b))
          (mainOfBenchmark b)
      | b <- benchmarks pd
      ]
    ]
  where
    libraryModules l = exposedModules l ++ otherModules (libBuildInfo l)

    -- The main library has no name of its own, and every other component does.
    nameOfLibrary l = case libName l of
      LMainLibName -> ComponentName "lib"
      LSubLibName n -> unqualName n

    unqualName = ComponentName . T.pack . unUnqualComponentName

    -- A cabal file says main-is as a string, so this is where one becomes a
    -- path. A name that is not a relative file is no module to look for.
    mainOfTest t = case testInterface t of
      Cabal.TestSuiteExeV10 _ p -> parseRelFile p
      _ -> Nothing

    mainOfBenchmark b = case Cabal.benchmarkInterface b of
      Cabal.BenchmarkExeV10 _ p -> parseRelFile p
      _ -> Nothing

data DeclaredComponent = DeclaredComponent
  { declaredKind :: !ComponentKind,
    declaredName :: !ComponentName,
    declaredBuildInfo :: !BuildInfo,
    declaredModules :: ![CabalModule.ModuleName],
    declaredMainIs :: !(Maybe (Path Rel File))
  }

componentOf :: SourceRoot -> Path Abs Dir -> DeclaredComponent -> IO ComponentModel
componentOf root dir declared = do
  let bi = declaredBuildInfo declared
  let kind = declaredKind declared
  let sourceDirs = [dir </> d | Just d <- map (parseRelDir . getSymbolicPath) (hsSourceDirs bi)]
  let wanted = filter (not . isAutogen) (declaredModules declared)
  found <- traverse (resolveModule root sourceDirs) wanted
  mainModule <- traverse (resolveMain root sourceDirs) (declaredMainIs declared)
  pure
    ComponentModel
      { componentModelKind = kind,
        componentModelName = declaredName declared,
        componentModelDefaultExtensions = map extensionText (defaultExtensions bi),
        componentModelModules = catMaybes found ++ catMaybes (catMaybes [mainModule]),
        componentModelDeclaredModules =
          map moduleKeyOf wanted
            ++ [ModuleKey "Main" | Just _ <- [declaredMainIs declared]],
        componentModelSourceDirs = sourceDirs
      }

-- | @Paths_foo@ and friends are generated at build time, so they are declared
-- in the cabal file and absent from the source tree.
isAutogen :: CabalModule.ModuleName -> Bool
isAutogen m = "Paths_" `isPrefixOf` intercalate "." (CabalModule.components m)

resolveModule :: SourceRoot -> [Path Abs Dir] -> CabalModule.ModuleName -> IO (Maybe ModuleModel)
resolveModule root sourceDirs m = do
  let relative = FP.joinPath (CabalModule.components m)
  let candidatesWith exts =
        [ d </> rel
        | d <- sourceDirs,
          ext <- exts,
          Just rel <- [parseRelFile (relative ++ ext)]
        ]
  haskell <- filterM isPresent (candidatesWith haskellExtensions)
  preprocessed <- filterM isPresent (candidatesWith preprocessorExtensions)
  -- Haskell first: a package may ship both, in which case the checked-in
  -- module is the one to read.
  pure $ case (haskell, preprocessed) of
    (f : _, _) -> modelFor HaskellSource f
    ([], f : _) -> modelFor PreprocessedSource f
    ([], []) -> Nothing
  where
    modelFor source f = do
      rp <- relPathIn root f
      pure
        ModuleModel
          { moduleModelKey = ModuleKey (T.pack (intercalate "." (CabalModule.components m))),
            moduleModelFile = f,
            moduleModelRelPath = rp,
            moduleModelSource = source
          }

-- | Whether a file is there, asked so that absence arrives as an answer rather
-- than as a 'Bool' from a separate question.
--
-- Which extension an author used is what is being asked here, so a stat is what
-- there is to ask: nothing is read until the module is parsed. 'forgivingAbsence'
-- rather than @doesFileExist@ all the same, because every other absence in this
-- tool arrives the same way and one that arrives as a 'Bool' is one somebody has
-- to remember to check.
isPresent :: Path Abs File -> IO Bool
isPresent f = isJust <$> forgivingAbsence (getModificationTime f)

haskellExtensions :: [String]
haskellExtensions = [".hs", ".lhs"]

-- | alex, happy and hsc2hs inputs. The module they declare is real and its
-- source is generated at build time, so it is neither a Haskell module to read
-- nor a module that vanished from the tree.
preprocessorExtensions :: [String]
preprocessorExtensions = [".x", ".y", ".hsc", ".chs"]

extensionText :: CabalExt.Extension -> Text
extensionText = \case
  CabalExt.EnableExtension k -> T.pack (show k)
  CabalExt.DisableExtension k -> T.pack ("No" ++ show k)
  CabalExt.UnknownExtension s -> T.pack s

-- | Haskell files under a package's source directories that no component
-- claims.
--
-- These are invisible to every layer, so a suppression written in one can
-- neither be used nor reported unused, which is the silent accumulation the
-- annotation mechanism exists to prevent.
unclaimedSourceFiles :: PackageModel -> IO [Path Abs File]
unclaimedSourceFiles pm = do
  let claimed = S.fromList [moduleModelFile m | c <- packageModelComponents pm, m <- componentModelModules c]
  let dirs = S.toList (S.fromList (concatMap componentModelSourceDirs (packageModelComponents pm)))
  present <- concat <$> traverse (haskellFilesUnder (packageModelDataPaths pm)) dirs
  pure (S.toAscList (S.difference (S.fromList present) claimed))

-- | Lenient decoding, because an undecodable byte sequence is not the same
-- problem as a module that does not parse, and conflating them would make the
-- parse-failure signal useless.
readSource :: Path Abs File -> IO Text
readSource f = TE.decodeUtf8With TE.lenientDecode <$> BS.readFile (toFilePath f)

-- | Whether a directory is, or is under, something the cabal file declares as
-- data.
isDataPath :: [Path Abs Dir] -> Path Abs Dir -> Bool
isDataPath dataPaths dir = any (\d -> d == dir || isProperPrefixOf d dir) dataPaths
  where
    isProperPrefixOf d x = case stripProperPrefix d x of
      Just _ -> True
      Nothing -> False

-- Two kinds of directory are somebody else's: one holding a cabal file is its
-- own package, and one the cabal file declares as data holds data. A declared
-- source directory need not exist, so absence is a normal answer.
haskellFilesUnder :: [Path Abs Dir] -> Path Abs Dir -> IO [Path Abs File]
haskellFilesUnder dataPaths dir
  | isDataPath dataPaths dir = pure []
  | otherwise = do
      listed <- forgivingAbsence (listDir dir)
      case listed of
        Nothing -> pure []
        Just (subs, files)
          | any (\f -> fileExtension f == Just ".cabal") files -> pure []
          | otherwise -> do
              deeper <- traverse (haskellFilesUnder dataPaths) subs
              pure (filter isHaskellSource files ++ concat deeper)
  where
    isHaskellSource f = fileExtension f `elem` map Just haskellExtensions

-- | Each @extra-source-files@ entry, up to its first glob, and only the
-- directory part of it. A package that declares something as extra source
-- files has said it is data rather than a module, and it is the directory that
-- has to be skipped.
dataPathsOf :: PackageDescription -> [Path Rel Dir]
dataPathsOf pd =
  [ dir
  | entry <- extraSrcFiles pd,
    let literal = takeWhile (not . isGlob) (FP.splitDirectories entry),
    not (null literal),
    Just dir <- [parseRelDir (FP.joinPath (dropLast literal))]
  ]
  where
    isGlob = any (`elem` ("*?[" :: String))

    -- The last component of a literal prefix is a file when the entry named one
    -- and a directory when the glob was cut before it. Dropping it keeps the
    -- type honest: what is skipped is a subtree.
    dropLast xs = take (length xs - 1) xs

moduleKeyOf :: CabalModule.ModuleName -> ModuleKey
moduleKeyOf m = ModuleKey (T.pack (intercalate "." (CabalModule.components m)))

-- | The module a component's @main-is@ names, which is always @Main@ however
-- the file is spelled.
resolveMain :: SourceRoot -> [Path Abs Dir] -> Path Rel File -> IO (Maybe ModuleModel)
resolveMain root sourceDirs mainIs = do
  let candidates = [d </> mainIs | d <- sourceDirs]
  existing <- filterM isPresent candidates
  pure $ case existing of
    (f : _) -> do
      rp <- relPathIn root f
      pure
        ModuleModel
          { moduleModelKey = ModuleKey "Main",
            moduleModelFile = f,
            moduleModelRelPath = rp,
            moduleModelSource =
              if fileExtension f `elem` map Just preprocessorExtensions
                then PreprocessedSource
                else HaskellSource
          }
    [] -> Nothing
