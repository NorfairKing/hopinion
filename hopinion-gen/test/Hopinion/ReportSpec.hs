{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | What a person actually sees, pinned as golden output.
--
-- The rendering is the product for anyone who is not reading the source, so it
-- gets reviewed like one: the golden files are there to be looked at, and a
-- change to them is a change to the thing being delivered.
module Hopinion.ReportSpec (spec) where

import Data.List (sort)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Hopinion.Annotation (OverBroad, Unused)
import Hopinion.Facts (PackageName (..))
import Hopinion.Facts.Gen ()
import Hopinion.Project (SourceRoot (..))
import Hopinion.Report
import Hopinion.Report.Render
import Hopinion.Rule (Finding)
import Hopinion.Rule.Gen (shippedRules)
import Hopinion.Run
import Path (Dir, Path, Rel, reldir, relfile, toFilePath, (</>))
import Path.IO (listDirRel, makeAbsolute)
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

resourceDir :: Path Rel Dir
resourceDir = [reldir|test_resources/Report|]

spec :: Spec
spec = do
  -- A report crosses a process boundary the same way facts do, so it is held to
  -- the same standard.
  describe "Finding" $ do
    genValidSpec @Finding
    jsonSpec @Finding
  describe "Unused" $ do
    genValidSpec @Unused
    jsonSpec @Unused
  describe "OverBroad" $ do
    genValidSpec @OverBroad
    jsonSpec @OverBroad
  describe "ParseFailure" $ do
    genValidSpec @ParseFailure
    jsonSpec @ParseFailure
  describe "StoreProblem" $ do
    genValidSpec @StoreProblem
    jsonSpec @StoreProblem
  describe "Failure" $ do
    genValidSpec @Failure
    jsonSpec @Failure
  describe "Complaint" $ do
    genValidSpec @Complaint
    jsonSpec @Complaint
  describe "Complaints" $ do
    genValidSpec @Complaints
    jsonSpec @Complaints
    it "round trips through the bytes a report directory holds" $
      forAllValid $ \complaints ->
        decodeReport (encodeReport complaints) `shouldBe` Right complaints
    -- The one question a wrapper deciding whether to fail has to ask.
    it "is clean exactly when it has nothing to complain about" $
      forAllValid $ \complaints ->
        isClean complaints `shouldBe` null (complaintList complaints)

  it "has the repository the goldens are of, and the goldens" $ do
    (dirs, files) <- listDirRel resourceDir
    sort dirs `shouldBe` [[reldir|repo|]]
    sort files `shouldBe` [[relfile|every-kind.golden|], [relfile|failure.golden|]]

  it "shows a finding, a broken suppression, an unused one and an over-broad one" $
    goldenTextFile (toFilePath (resourceDir </> [relfile|every-kind.golden|])) $ do
      root <- rootAt (resourceDir </> [reldir|repo|])
      report <- runCheck shippedRules noHieDirectories root
      (sources, missing) <- sourcesForReport [root] report
      pure (renderReport shippedRules sources (report <> missing))

  -- Every golden here is the plain rendering, because a golden full of escape
  -- sequences is not one a person can review. So this is what says the other
  -- rendering exists and is the one a terminal gets: without it, 'printReport'
  -- could hand a terminal the plain text and every golden would still match.
  it "colours the report for a terminal and not for a pipe" $ do
    root <- rootAt (resourceDir </> [reldir|repo|])
    complaints <- runCheck shippedRules noHieDirectories root
    (sources, missing) <- sourcesForReport [root] complaints
    let together = complaints <> missing
    renderReportColoured shippedRules sources together `shouldSatisfy` T.isInfixOf "\ESC"
    renderReport shippedRules sources together `shouldSatisfy` not . T.isInfixOf "\ESC"

  -- A failure is what the tool could not do rather than what is wrong with the
  -- code, so it has no position and nothing to show underneath.
  it "shows a failure with no code under it" $
    pureGoldenTextFile (toFilePath (resourceDir </> [relfile|failure.golden|])) $
      renderReport
        shippedRules
        (SourceMap M.empty)
        (failureComplaints [FactsIncomplete (NoFactsForPackage (PackageName "lonely"))])

-- | A source root over a directory, resolved against the working directory the
-- suite runs in, which is the package directory.
rootAt :: Path Rel Dir -> IO SourceRoot
rootAt dir = do
  absDir <- makeAbsolute dir
  pure SourceRoot {sourceRootDir = absDir, sourceRootPrefix = Nothing}
