{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hopinion.RunSpec (spec) where

import Data.List (sort)
import qualified Data.Text as T
import Hopinion.Rule (renderRuleSetError, withoutRules)
import Hopinion.Rule.Gen (shippedRules)
import Hopinion.Rule.Id
import Hopinion.Run
import Path (Dir, Path, Rel, reldir, relfile, toFilePath, (</>))
import Path.IO (listDirRel)
import Test.Syd
import Text.Colour (TerminalCapabilities (..), renderChunksText)

resourceDir :: Path Rel Dir
resourceDir = [reldir|test_resources/Run|]

spec :: Spec
spec = describe "explainRule" $ do
  -- The two a reader is shown prose for. The third thing a name can turn out to
  -- be is a name nothing answers to, whose whole text this module owns and is
  -- asserted exactly below.
  it "has a golden for each rule an explanation can be about" $ do
    (dirs, files) <- listDirRel resourceDir
    dirs `shouldBe` []
    sort files `shouldBe` [[relfile|runs.golden|], [relfile|turned-off.golden|]]

  -- What a reader is shown is external output, so it is goldened rather than
  -- restated here out of the same fields the code assembles it from. The words
  -- are goldened without the colour, which is what a reader reviews and what a
  -- caller capturing the output gets; that any colour is asked for at all is
  -- asserted separately below.
  it "explains a rule this run makes" $
    goldenTextFile (toFilePath (resourceDir </> [relfile|runs.golden|])) $
      pure $
        renderChunksText WithoutColours $ case explainRule shippedRules (RuleId "CommentBareTodo") of
          Explained cs -> cs
          NoRuleCalled cs -> cs

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
          pure $ renderChunksText WithoutColours $ case explainRule rs (RuleId "CommentBareTodo") of
            Explained cs -> cs
            NoRuleCalled cs -> cs

  -- A name nothing answers to is a question this build cannot answer, so it is
  -- refused rather than answered with prose that reads like an explanation.
  it "refuses a name nothing answers to" $
    case explainRule shippedRules (RuleId "NoSuchRule") of
      Explained cs -> expectationFailure (unwords ["Explained a rule this build does not have:", show cs])
      NoRuleCalled cs ->
        renderChunksText WithoutColours cs
          `shouldBe` "There is no rule called NoSuchRule in this build of hopinion.\n\
                     \Run list-rules to see the ones there are.\n"
