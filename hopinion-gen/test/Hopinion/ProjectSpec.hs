{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hopinion.ProjectSpec (spec) where

import Data.List (sort)
import Hopinion.Facts.Name (ComponentName (..), ModuleKey (..), PackageName (..))
import Hopinion.Facts.Place (ModuleRef (..))
import Hopinion.Project
import Hopinion.Report
import Hopinion.Rule.Gen (shippedRules)
import Hopinion.Run
import Path (Dir, Path, Rel, reldir, relfile, toFilePath, (</>))
import Path.IO (createDirIfMissing, createDirLink, listDirRel, makeAbsolute, withSystemTempDir)
import Test.Syd

resourceDir :: Path Rel Dir
resourceDir = [reldir|test_resources/Project|]

spec :: Spec
spec = do
  it "has a project for each way discovery can be given something it cannot read" $ do
    (dirs, files) <- listDirRel resourceDir
    files `shouldBe` []
    sort dirs
      `shouldBe` [ [reldir|absent-module|],
                   [reldir|absent-source-dir|],
                   [reldir|duplicate-package|],
                   [reldir|name-lookalike|]
                 ]

  describe "skippedByName" $ do
    -- cabal unpacks a source-repository-package into dist-newstyle/src, cabal
    -- file and all, so a package found under there belongs to a dependency.
    it "skips the build directory that holds unpacked dependencies" $
      skippedByName [reldir|dist-newstyle|] `shouldBe` True

    it "skips a hidden directory whatever it is called" $ do
      skippedByName [reldir|.git|] `shouldBe` True
      skippedByName [reldir|.stack-work|] `shouldBe` True
      skippedByName [reldir|.direnv|] `shouldBe` True

    -- The whole name, not a prefix of it. Asking by prefix skipped `results`
    -- along with `result`, and a package under it went missing from discovery
    -- with nothing said about it.
    it "walks a directory whose name merely starts like one it skips" $ do
      skippedByName [reldir|results|] `shouldBe` False
      skippedByName [reldir|result|] `shouldBe` False
      skippedByName [reldir|dist-newstyle-backup|] `shouldBe` False

    it "walks an ordinary directory" $ do
      skippedByName [reldir|src|] `shouldBe` False
      skippedByName [reldir|hopinion-gen|] `shouldBe` False

    -- Named on purpose, so that adding a name back is a decision rather than a
    -- default. Nothing puts a Haskell package under either.
    it "walks the directories it deliberately does not name" $ do
      skippedByName [reldir|node_modules|] `shouldBe` False
      skippedByName [reldir|result|] `shouldBe` False

  -- The regression for that prefix test, over a real walk rather than the
  -- predicate: the package is two levels under a directory called `results`.
  it "discovers a package under a directory whose name starts like one it skips" $ do
    root <- rootAt (resourceDir </> [reldir|name-lookalike|])
    models <- discoverPackages root (sourceRootDir root)
    fmap (map packageModelName) models `shouldBe` Right [PackageName "thing"]

  -- A symlink is walked like any other directory. A repository that links a
  -- package in has one to be found, and a nix build output, which is the symlink
  -- anybody actually has in a working tree, holds no cabal file to find.
  it "finds a package a repository links in" $
    withSystemTempDir "hopinion-symlink" $ \tmp -> do
      let elsewhere = tmp </> [reldir|elsewhere|]
      createDirIfMissing True (elsewhere </> [reldir|thing|])
      writeFile (toFilePath (elsewhere </> [reldir|thing|] </> [relfile|thing.cabal|])) minimalCabal
      let repo = tmp </> [reldir|repo|]
      createDirIfMissing True repo
      createDirLink (elsewhere </> [reldir|thing|]) (repo </> [reldir|linked-thing|])
      let root = SourceRoot {sourceRootDir = repo, sourceRootPrefix = Nothing}
      models <- discoverPackages root repo
      fmap (map packageModelName) models `shouldBe` Right [PackageName "thing"]

  -- The root a command is pointed at may not be there either, which is a
  -- mistake in whatever named it. It has to be a complaint rather than an
  -- exception for the reason every other one does: a producing command that
  -- dies has written no report, and producing one always succeeds.
  it "says so when the root is not a directory, rather than throwing" $ do
    root <- rootAt (resourceDir </> [reldir|no-such-directory|])
    result <- discoverPackages root (sourceRootDir root)
    result `shouldBe` Left (NoDirectoryAt (sourceRootDir root))

  it "carries that into the report rather than killing the run" $ do
    root <- rootAt (resourceDir </> [reldir|no-such-directory|])
    report <- runCheck shippedRules noHieDirectories root
    [t | ComplaintFailure t <- complaintList report]
      `shouldBe` [RepositoryUnreadable (renderDiscoveryError (NoDirectoryAt (sourceRootDir root)))]

  -- A cabal file may declare a source directory that is not there, and sydtest
  -- does. Walking it for unclaimed modules must be an empty answer rather than
  -- an exception, which would kill the run.
  it "reads a package whose cabal file declares a source directory that is not there" $ do
    let dir = resourceDir </> [reldir|absent-source-dir|]
    report <- runCheck shippedRules noHieDirectories =<< rootAt dir
    report `shouldBe` mempty

  -- Absent input is an error, never an empty set.
  --
  -- A module the cabal file declares and the tree does not have would otherwise
  -- vanish from the index, and every project rule would then reason over an
  -- instance table with a hole in it and find nothing wrong.
  it "fails when a declared module is missing from the tree, naming it" $ do
    let dir = resourceDir </> [reldir|absent-module|]
    report <- runCheck shippedRules noHieDirectories =<< rootAt dir
    [t | ComplaintFailure t <- complaintList report]
      `shouldBe` [ FactsIncomplete
                     ( PackageDoesNotCover
                         (PackageName "thing")
                         ModuleRef
                           { moduleRefModule = ModuleKey "Vanished",
                             moduleRefComponent = ComponentName "lib"
                           }
                     )
                 ]

  -- A package name is what every fact is filed under, so two cabal files
  -- claiming one name is a repository nothing can reason about. A complaint
  -- about the repository, because the alternatives are a quiet overwrite and a
  -- constraint violation from inside the store.
  it "fails when two packages are called the same thing, naming both cabal files" $ do
    let dir = resourceDir </> [reldir|duplicate-package|]
    report <- runCheck shippedRules noHieDirectories =<< rootAt dir
    [renderFailure t | ComplaintFailure t <- complaintList report]
      `shouldBe` [ "Two packages are called thing and one name is all the facts of either \
                   \can be filed under: first/thing.cabal second/thing.cabal"
                 ]

-- | A source root over a directory, resolved against the working directory the
-- suite runs in, which is the package directory.
rootAt :: Path Rel Dir -> IO SourceRoot
rootAt dir = do
  absDir <- makeAbsolute dir
  pure SourceRoot {sourceRootDir = absDir, sourceRootPrefix = Nothing}

-- | The least a cabal file can say and still name a package.
minimalCabal :: String
minimalCabal =
  unlines
    [ "cabal-version: 1.12",
      "name:          thing",
      "version:       0.0.0",
      "build-type:    Simple"
    ]
