{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hopinion.ProjectSpec (spec) where

import Data.List (sort)
import Hopinion.Facts.Name (ComponentName (..), ModuleKey (..), PackageName (..))
import Hopinion.Facts.Place (ModuleRef (..))
import Hopinion.Project
import Hopinion.Report
import Hopinion.Rule.Gen (shippedRules)
import Hopinion.Run (HieDirectories (..), runPackageCommand)
import Hopinion.TestUtils (checkBuilt)
import Path (Dir, Path, Rel, reldir, (</>))
import Path.IO (listDirRel, makeAbsolute, withSystemTempDir)
import Test.Syd

spec :: Spec
spec = do
  let resourceDir :: Path Rel Dir
      resourceDir = [reldir|test_resources/Project|]

  -- A source root over a directory, resolved against the working directory the
  -- suite runs in, which is the package directory.
  let rootAt :: Path Rel Dir -> IO SourceRoot
      rootAt dir = do
        absDir <- makeAbsolute dir
        pure
          SourceRoot
            { sourceRootDir = absDir,
              sourceRootPrefix = Nothing
            }

  it "has a project for each way a package can be given something it cannot read" $ do
    (dirs, files) <- listDirRel resourceDir
    files `shouldBe` []
    sort dirs
      `shouldBe` [ [reldir|absent-module|],
                   [reldir|absent-source-dir|],
                   [reldir|detailed-suite|]
                 ]

  -- A detailed suite names its entry point in test-module and nowhere else, so
  -- reading only other-modules would leave every rule blind to it.
  it "reads a detailed suite's test-module as one of its modules" $ do
    root <- rootAt (resourceDir </> [reldir|detailed-suite|])
    result <- readPackageIn root (sourceRootDir root </> [reldir|thing|])
    fmap (map componentModelDeclaredModules . packageModelComponents) result
      `shouldBe` Right [[ModuleKey "Helper", ModuleKey "Tests"]]

  -- The directory a command is pointed at may not be there, which is a mistake
  -- in whatever named it. It has to be a complaint rather than an exception for
  -- the reason every other one does: a producing command that dies has written
  -- no report, and producing one always succeeds.
  it "says so when the package directory is not there, rather than throwing" $ do
    root <- rootAt resourceDir
    let dir = sourceRootDir root </> [reldir|no-such-directory|]
    result <- readPackageIn root dir
    fmap packageModelName result `shouldBe` Left (DiscoveryErrorNoDirectoryAt dir)

  it "carries that into the report rather than killing the run" $
    withSystemTempDir "hopinion-absent" $ \tmp -> do
      root <- rootAt resourceDir
      let dir = sourceRootDir root </> [reldir|no-such-directory|]
      report <- runPackageCommand shippedRules (HieDirectories (pure tmp)) root dir (tmp </> [reldir|out|])
      [t | ComplaintFailure t <- unComplaints report]
        `shouldBe` [FailurePackageUnreadable (renderDiscoveryError (DiscoveryErrorNoDirectoryAt dir))]

  -- A cabal file may declare a source directory that is not there, and sydtest
  -- does. Walking it for unclaimed modules must be an empty answer rather than
  -- an exception, which would kill the run.
  it "reads a package whose cabal file declares a source directory that is not there" $ do
    let dir = resourceDir </> [reldir|absent-source-dir|]
    report <- checkBuilt shippedRules =<< rootAt dir
    report `shouldBe` mempty

  -- Absent input is an error, never an empty set.
  --
  -- A module the cabal file declares and the tree does not have would otherwise
  -- vanish from the index, and every project rule would then reason over an
  -- instance table with a hole in it and find nothing wrong.
  it "fails when a declared module is missing from the tree, naming it" $ do
    let dir = resourceDir </> [reldir|absent-module|]
    report <- checkBuilt shippedRules =<< rootAt dir
    [t | ComplaintFailure t <- unComplaints report]
      `shouldBe` [ FailureFactsIncomplete
                     ( StoreProblemPackageDoesNotCover
                         (PackageName "thing")
                         ModuleRef
                           { moduleRefModule = ModuleKey "Vanished",
                             moduleRefComponent = ComponentName "lib"
                           }
                     )
                 ]
