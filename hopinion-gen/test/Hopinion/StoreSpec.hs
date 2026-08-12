{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hopinion.StoreSpec (spec) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (get, insertKey)
import Database.Persist.Sql (rawExecute)
import Hopinion.Facts
import Hopinion.Rule.Id (RuleId (..))
import Hopinion.Store
import Path (File, Path, Rel, relfile, toFilePath, (</>))
import Path.IO (withSystemTempDir)
import Test.Syd

spec :: Spec
spec = do
  -- A rule brings its own table, so a store written where a rule existed and
  -- merged where it does not is a merge with nowhere to put a row. Left to
  -- SQLite it dies saying `no such table`, which is a sentence about a name
  -- nobody chose from a run that has not written its report yet.
  --
  -- Turning a rule off does not reach this, since a store is migrated for every
  -- rule its executable has rather than for the ones a run makes. What is left
  -- is two different executables over one set of facts, and this is that.
  describe "mergeStore" $ do
    it "says which tables it has nowhere to put, rather than failing on a row" $
      withSystemTempDir "hopinion-store" $ \tmp -> do
        let foreign' = tmp </> [relfile|foreign.db|]
        -- Raw, because what a rule's table is called is that rule's business
        -- and this is about a table this build has never heard of, which is
        -- what an executable with a rule this one lacks leaves behind.
        withStore [] (OnDisk foreign') $ do
          rawExecute "CREATE TABLE \"a_rule_we_do_not_have\" (\"id\" INTEGER PRIMARY KEY, \"what\" VARCHAR NOT NULL)" []
          rawExecute "INSERT INTO \"a_rule_we_do_not_have\" (\"what\") VALUES ('something')" []
        unmergeable <- withMemoryStore [] (mergeStore foreign')
        unmergeable
          `shouldBe` [ T.concat
                         [ "The facts in ",
                           T.pack (toFilePath foreign'),
                           " hold a_rule_we_do_not_have, which this build of hopinion has no rule for",
                           " and so no table for. The package outputs and this run were made by",
                           " different executables."
                         ]
                     ]

    it "copies every table a store holds when this one has them all" $
      withSystemTempDir "hopinion-store" $ \tmp -> do
        let foreign' = tmp </> [relfile|foreign.db|]
        withStore [] (OnDisk foreign') $
          insertKey
            (StoredPackageKey (PackageName "thing"))
            StoredPackage
              { storedPackageName = PackageName "thing",
                storedPackageRole = RoleMain,
                storedPackageCabal = StoredPath [relfile|thing/thing.cabal|]
              }
        merged <- withMemoryStore [] $ do
          unmergeable <- mergeStore foreign'
          names <- packageNames
          pure (unmergeable, names)
        merged `shouldBe` ([], [PackageName "thing"])

  describe "writtenByAnotherVersion" $ do
    it "says no about a store this tool stamped" $ do
      other <- withMemoryStore [] $ do
        writeMeta
        writtenByAnotherVersion
      other `shouldBe` False
    it "says no about a store nobody stamped, because there is nothing there to disagree" $ do
      other <- withMemoryStore [] writtenByAnotherVersion
      other `shouldBe` False
    it "says yes about a store stamped with another format version" $ do
      other <- withMemoryStore [] $ do
        let format = formatVersionNumber currentFormatVersion + 1
        insertKey (StoredMetaKey format) StoredMeta {storedMetaFormatVersion = format}
        writtenByAnotherVersion
      other `shouldBe` True
    -- Through the merge, which is the only path that ever puts a foreign stamp
    -- in a store, and the path a tool version beside the format could not
    -- survive: keyed on the format, a second store's row at this same format is
    -- dropped by the insert.
    it "says yes about a store merged from one written in another format" $
      withSystemTempDir "hopinion-store" $ \tmp -> do
        let foreign' = tmp </> [relfile|foreign.db|]
        withStore [] (OnDisk foreign') $ do
          let format = formatVersionNumber currentFormatVersion + 1
          insertKey (StoredMetaKey format) StoredMeta {storedMetaFormatVersion = format}
        other <- withMemoryStore [] $ do
          writeMeta
          _ <- mergeStore foreign'
          writtenByAnotherVersion
        other `shouldBe` True

  -- The envelope is what a package writes about itself before any rule writes
  -- a row: which package it is, which modules it was held to covering, and
  -- what each module turned out to be. A project run reads it back out of the
  -- file the package run left, so what is written has to be what comes back.
  describe "writePackageEnvelope" $ do
    it "writes a package a later query finds by name" $ do
      names <- withMemoryStore [] $ do
        writePackageEnvelope
          (PackageName "thing")
          RoleMain
          [relfile|thing/thing.cabal|]
          [refIn "lib" "Thing"]
        packageNames
      names `shouldBe` [PackageName "thing"]

    -- The whole row, because the path and the role only ever go one way
    -- otherwise: every query here selects the name, so nothing else would run
    -- the parser that reads a stored path back and says no to one that is not
    -- a repository-relative file.
    it "writes a package that reads back as the row it wrote" $ do
      stored <- withMemoryStore [] $ do
        writePackageEnvelope (PackageName "thing") RoleGen [relfile|thing/thing.cabal|] []
        get (StoredPackageKey (PackageName "thing"))
      stored
        `shouldBe` Just
          StoredPackage
            { storedPackageName = PackageName "thing",
              storedPackageRole = RoleGen,
              storedPackageCabal = StoredPath [relfile|thing/thing.cabal|]
            }

    -- A second row for one key is a constraint violation rather than a silent
    -- overwrite, which is what the schema is keyed for. It is not a report:
    -- writing one is a mistake in whatever enumerated the packages, and
    -- discovery refuses two packages by one name before it gets here. This says
    -- so out loud, so that a caller that starts producing one is not the thing
    -- that discovers it.
    it "refuses a second package under one name" $ do
      thrown <-
        try
          ( withMemoryStore [] $ do
              writePackageEnvelope (PackageName "thing") RoleMain [relfile|a/thing.cabal|] []
              writePackageEnvelope (PackageName "thing") RoleMain [relfile|b/thing.cabal|] []
          )
      renderedOf thrown `shouldSatisfy` T.isInfixOf "UNIQUE constraint failed: stored_package.name"

  describe "writeModuleEnvelope" $ do
    -- Two packages, so that the query answering with one package's suppressions
    -- is the thing under test rather than an empty table.
    it "writes the suppressions of one module and reads back only that package's" $ do
      stored <- withMemoryStore [] $ do
        writePackageEnvelope (PackageName "thing") RoleMain [relfile|thing/thing.cabal|] []
        writePackageEnvelope (PackageName "other") RoleMain [relfile|other/other.cabal|] []
        writeModuleEnvelope
          (PackageName "thing")
          (refIn "lib" "Thing")
          [relfile|thing/src/Thing.hs|]
          ParsedOk
          [suppressionOf "CommentBareTodo"]
        writeModuleEnvelope
          (PackageName "other")
          (refIn "lib" "Other")
          [relfile|other/src/Other.hs|]
          ParsedOk
          [suppressionOf "HsNoCustomShowRead"]
        annotationsOfPackage (PackageName "thing")
      map annotationFactRule stored `shouldBe` [RuleId "CommentBareTodo"]

    it "refuses a second module under one reference" $ do
      thrown <-
        try
          ( withMemoryStore [] $ do
              writePackageEnvelope (PackageName "thing") RoleMain [relfile|thing/thing.cabal|] []
              writeModuleEnvelope (PackageName "thing") (refIn "lib" "Thing") [relfile|a.hs|] ParsedOk []
              writeModuleEnvelope (PackageName "thing") (refIn "lib" "Thing") [relfile|b.hs|] ParsedOk []
          )
      renderedOf thrown
        `shouldSatisfy` T.isInfixOf "UNIQUE constraint failed: stored_module.package, stored_module.module_ref"

  -- A fact stored whole goes into its column through the codec it carries, so a
  -- column holding something else is a store nothing can read. It says which
  -- value it could not read, which is the only thing a reader can act on.
  describe "annotationsOfPackage" $
    it "says which value it cannot read when a column holds something that is not a fact" $ do
      thrown <-
        try
          ( withMemoryStore [] $ do
              writePackageEnvelope (PackageName "thing") RoleMain [relfile|thing/thing.cabal|] []
              rawExecute
                "INSERT INTO stored_annotation (package, module_ref, annotation) VALUES ('thing', 'lib:Thing', 'not a fact')"
                []
              annotationsOfPackage (PackageName "thing")
          )
      renderedOf thrown `shouldSatisfy` T.isInfixOf "not a fact"

-- | What an exception says, which is as much as these tests look at: the
-- wording is SQLite's and persistent's rather than this tool's.
renderedOf :: Either SomeException a -> Text
renderedOf = either (T.pack . show) (const "")

-- | A module of a component, which is what a stored row is keyed on.
refIn :: Text -> Text -> ModuleRef
refIn component m =
  ModuleRef {moduleRefComponent = ComponentName component, moduleRefModule = ModuleKey m}

-- | A suppression naming a rule, which is all these tests look at.
suppressionOf :: Text -> AnnotationFact
suppressionOf rule =
  AnnotationFact
    { annotationFactRule = RuleId rule,
      annotationFactScope = ScopeOfFile (refIn "lib" "Thing"),
      annotationFactPrecision = PrecisionFile,
      annotationFactReason = ReasonAdoption,
      annotationFactSpan = spanOfLine [relfile|thing/src/Thing.hs|] 1
    }

-- | One line of a file, which is as much of a span as these tests need.
spanOfLine :: Path Rel File -> Word -> Span
spanOfLine file line =
  Span
    { spanFile = file,
      spanStart = Position {positionLine = line, positionCol = 1},
      spanEnd = Position {positionLine = line, positionCol = 2}
    }
