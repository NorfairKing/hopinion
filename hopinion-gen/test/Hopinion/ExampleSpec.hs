{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The two example repositories the Nix end to end tests drive through the
-- derivations, checked here in one process first.
--
-- Everything nix/e2e.nix asserts about these projects rests on properties of
-- the projects themselves: which packages they hold, which layer each finding
-- comes from, and which file the path check greps for. Those properties are
-- cheap to check here and expensive to check there, so they are checked here,
-- and a change to an example fails in seconds rather than after a Nix build.
module Hopinion.ExampleSpec (spec) where

import Data.List (nub, sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Hopinion.Facts
import Hopinion.Project
import Hopinion.Report
import Hopinion.Report.Render
import Hopinion.Rule
import Hopinion.Rule.Gen (shippedRules)
import Hopinion.Rule.Id
import Hopinion.Run
import Path (Dir, Path, Rel, reldir, relfile, toFilePath, (</>))
import Path.IO (listDirRel, makeAbsolute)
import Test.Syd

resourceDir :: Path Rel Dir
resourceDir = [reldir|test_resources/Example|]

spec :: Spec
spec = do
  it "has the three examples nix/e2e.nix names, and the golden of the dirty one" $ do
    (dirs, files) <- listDirRel resourceDir
    sort dirs `shouldBe` [[reldir|clean|], [reldir|decided|], [reldir|dirty|]]
    sort files `shouldBe` [[relfile|dirty.golden|]]

  -- A repository decides which rules it is held to in one file at its root, and
  -- `check` is the command handed a repository, so it is the one that finds it.
  -- This is the whole of that path bar the command line: the file is read, the
  -- rule it names stops running, and the finding it would have made is gone.
  describe "decided" $ do
    it "does not run the rule its hopinion.yaml decided against" $ do
      report <- runCheck shippedRules noHieDirectories =<< rootAt (resourceDir </> [reldir|decided|])
      report `shouldBe` mempty

    -- The same repository with nothing decided, so that the test above is known
    -- to be about the file rather than about a repository with nothing wrong in
    -- it. Read from the same tree with the file left out of the picture, which
    -- is what a rule set that never had the repository's say applied is.
    it "would report it, so the file is what made the difference" $ do
      root <- rootAt (resourceDir </> [reldir|decided|])
      report <- runCheckWithoutChoices shippedRules noHieDirectories root
      map findingRule (complaintsFindings report)
        `shouldBe` [RuleId "CommentBareTodo"]

  describe "clean" $
    it "passes every check, which is what the Nix builders assert of it" $ do
      report <- runCheck shippedRules noHieDirectories =<< rootAt (resourceDir </> [reldir|clean|])
      report `shouldBe` mempty

  describe "dirty" $ do
    -- The Nix check compares eval-time discovery against this same list, so it
    -- is stated here too rather than only there.
    it "holds the three packages discovery must find" $ do
      let dir = resourceDir </> [reldir|dirty|]
      root <- rootAt dir
      eModels <- discoverPackages root (sourceRootDir root)
      case eModels of
        Left err -> expectationFailure (T.unpack (renderDiscoveryError err))
        Right models ->
          sort (map packageModelName models)
            `shouldBe` [PackageName "lonely", PackageName "thing", PackageName "thing-gen"]

    -- Each builder needs something to fail on, and they fail at different
    -- levels, so an example that lost a level would leave one of them green
    -- whatever the tool did.
    it "fails at every level" $ do
      report <- runCheck shippedRules noHieDirectories =<< rootAt (resourceDir </> [reldir|dirty|])
      [t | ComplaintFailure t <- complaintList report] `shouldBe` []
      sort (nub (map ruleLevel (mapMaybe (ruleFor shippedRules . findingRule) (complaintsFindings report))))
        `shouldBe` [LevelModule, LevelPackage, LevelProject]

    -- The golden is where the file name the Nix path check greps for is
    -- visible, so a rename of it here shows up as a change to this file.
    it "reports what the Nix checks read back" $
      goldenTextFile
        (toFilePath (resourceDir </> [relfile|dirty.golden|]))
        (renderedReportFor (resourceDir </> [reldir|dirty|]))

renderedReportFor :: Path Rel Dir -> IO Text
renderedReportFor dir = do
  root <- rootAt dir
  report <- runCheck shippedRules noHieDirectories root
  (sources, missing) <- sourcesForReport [root] report
  pure (renderReport shippedRules sources (report <> missing))

-- | A source root over a directory, resolved against the working directory the
-- suite runs in, which is the package directory.
rootAt :: Path Rel Dir -> IO SourceRoot
rootAt dir = do
  absDir <- makeAbsolute dir
  pure SourceRoot {sourceRootDir = absDir, sourceRootPrefix = Nothing}
