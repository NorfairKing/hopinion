{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Hopinion.RunSpec (spec) where

import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hopinion.Project (SourceRoot (..))
import Hopinion.Report.Render (sourcesForReport, writeReportTo)
import Hopinion.Rule (renderRuleSetError, ruleId, ruleLevel, ruleSetRules, rulesAtLevel, withoutRules)
import Hopinion.Rule.Gen (shippedRules)
import Hopinion.Rule.Id
import Hopinion.Run
import Hopinion.Timing (timingFile)
import Path (Dir, Path, Rel, reldir, relfile, toFilePath, (</>))
import Path.IO (ensureDir, listDirRel, resolveDir', withSystemTempDir)
import Test.Syd
import Text.Colour (TerminalCapabilities (..), renderChunksText)
import Text.Read (readMaybe)

spec :: Spec
spec = do
  describe "runPackageCommand and runProjectCommand" $ do
    -- Every step a run made is named, and nothing else is, so the file says
    -- where a slow run spent its time and never invents a step.
    it "write how long each step took, one line per step" $
      withSystemTempDir "hopinion-timing" $ \tmp -> do
        root <- resolveDir' "test_resources/Example/clean"
        let sourceRoot =
              SourceRoot
                { sourceRootDir = root,
                  sourceRootPrefix = Nothing
                }
        let packageOut = tmp </> [reldir|package|]
        let projectOut = tmp </> [reldir|project|]
        ensureDir packageOut
        ensureDir projectOut
        packageReport <- runPackageCommand shippedRules noHieDirectories sourceRoot (root </> [reldir|lonely|]) (Just packageOut)
        (sources, _) <- sourcesForReport [sourceRoot] packageReport
        writeReportTo shippedRules packageOut sources packageReport
        _ <- runProjectCommand shippedRules noHieDirectories [packageOut] ["lonely"] (Just projectOut)
        packageTimings <- TIO.readFile (toFilePath (packageOut </> timingFile))
        projectTimings <- TIO.readFile (toFilePath (projectOut </> timingFile))
        let steps = map (T.drop 1 . T.dropWhile (/= '\t')) . T.lines
        let seconds = map (readMaybe @Double . T.unpack . T.takeWhile (/= '\t')) . T.lines
        let named prefix rules = [T.concat [prefix, unRuleId (ruleId r)] | r <- rules]
        -- A duration is never the same twice, so what is asserted is that each
        -- line starts with one.
        map (fmap (>= 0)) (seconds packageTimings ++ seconds projectTimings)
          `shouldBe` map (const (Just True)) (T.lines packageTimings ++ T.lines projectTimings)
        steps packageTimings
          `shouldBe` concat
            [ ["read modules"],
              named "module rule " (rulesAtLevel shippedRules LevelModule),
              named "carry " [r | r <- ruleSetRules shippedRules, ruleLevel r /= LevelModule],
              named "package rule " (rulesAtLevel shippedRules LevelPackage),
              ["total"]
            ]
        steps projectTimings
          `shouldBe` concat
            [ ["merge package facts"],
              named "project rule " (rulesAtLevel shippedRules LevelProject),
              ["total"]
            ]

  describe "explainRule" $ do
    let resourceDir :: Path Rel Dir
        resourceDir = [reldir|test_resources/Run|]

    -- The two a reader is shown prose for. The third thing a name can turn out to
    -- be is a name nothing answers to, whose whole text this module owns and is
    -- asserted exactly below.
    it "has a golden for each rule an explanation can be about" $ do
      (dirs, files) <- listDirRel resourceDir
      dirs `shouldBe` []
      sort files `shouldBe` [[relfile|runs.golden|], [relfile|turned-off.golden|]]

    -- What a reader is shown is external output, so it is goldened rather than
    -- restated here out of the same fields the code assembles it from. Goldened
    -- with the colour, because the colour is part of what is shown: which words
    -- are picked out and which are not is a decision, and one nothing else
    -- reviews. Read these with something that interprets the escapes.
    it "explains a rule this run makes" $
      goldenTextFile (toFilePath (resourceDir </> [relfile|runs.golden|])) $
        pure $
          renderChunksText With8BitColours $ case explainRule shippedRules (RuleId "CommentBareTodo") of
            ExplanationGiven cs -> cs
            ExplanationNoRuleCalled cs -> cs

    -- The why and the fix answer different questions, so each says which one
    -- it answers.
    it "labels why a rule exists and what to do instead" $
      case explainRule shippedRules (RuleId "CommentBareTodo") of
        ExplanationNoRuleCalled cs -> expectationFailure (unwords ["Did not explain a rule this build has:", show cs])
        ExplanationGiven cs ->
          renderChunksText WithoutColours cs
            `shouldBe` "CommentBareTodo\n\
                       \A TODO names an issue or a URL.\n\
                       \Why: A TODO on its own reads like a plan and goes stale in place.\n\
                       \A reference says where the work is tracked, and a made-up one points nowhere.\n\
                       \Instead: If the work is small and part of your task, do it and delete the TODO.\n\
                       \Otherwise, add a URL, an issue number such as `#412`, or a ticket such as `FOO-412` to the same comment.\n\
                       \Do not invent one: if no issue exists, ask whether to file one.\n\
                       \Level: module\n"

    it "asks for colour when the terminal has it" $
      renderChunksText With8BitColours (listRules shippedRules)
        `shouldNotBe` renderChunksText WithoutColours (listRules shippedRules)

    -- Colour and nothing else. safe-coloured-text emits bold, faint, italic and
    -- underline whatever the terminal can do, and only a colour disappears when it
    -- cannot, so styling with one of those would put escape sequences in every
    -- build log that captures this stream.
    it "says nothing in escape sequences when the terminal has no colour" $
      renderChunksText WithoutColours (listRules shippedRules)
        `shouldSatisfy` not . T.isInfixOf "\ESC"

    -- A rule the repository turned off is still a rule this build has, so saying
    -- there is no such rule would send a reader looking for a typo they did not
    -- make.
    it "explains a rule this repository has turned off, and says it is off" $
      goldenTextFile (toFilePath (resourceDir </> [relfile|turned-off.golden|])) $
        case withoutRules [RuleId "CommentBareTodo"] shippedRules of
          Left err -> fail (T.unpack (renderChunksText WithoutColours (renderRuleSetError err)))
          Right rs ->
            pure $ renderChunksText With8BitColours $ case explainRule rs (RuleId "CommentBareTodo") of
              ExplanationGiven cs -> cs
              ExplanationNoRuleCalled cs -> cs

    -- A name nothing answers to is a question this build cannot answer, so it is
    -- refused rather than answered with prose that reads like an explanation.
    it "refuses a name nothing answers to" $
      case explainRule shippedRules (RuleId "NoSuchRule") of
        ExplanationGiven cs -> expectationFailure (unwords ["Explained a rule this build does not have:", show cs])
        ExplanationNoRuleCalled cs ->
          renderChunksText WithoutColours cs
            `shouldBe` "There is no rule called NoSuchRule in this build of hopinion.\n\
                       \Run list-rules to see the rules this build has.\n"
