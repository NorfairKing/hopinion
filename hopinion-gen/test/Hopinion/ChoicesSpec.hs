{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Hopinion.ChoicesSpec (spec) where

import Autodocodec.Yaml (renderPlainSchemaViaCodec)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Yaml as Yaml
import Hopinion.Choices
import Hopinion.Facts.Gen ()
import Hopinion.Rule.Id
import Path (Dir, Path, Rel, absfile, reldir, relfile, toFilePath, (</>))
import Path.IO (listDirRel)
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

resourceDir :: Path Rel Dir
resourceDir = [reldir|test_resources/Choices|]

spec :: Spec
spec = do
  it "has the goldens it reads and no others" $ do
    (dirs, files) <- listDirRel resourceDir
    dirs `shouldBe` []
    sort files `shouldBe` [[relfile|schema.txt|]]

  describe "Choices" $ do
    genValidSpec @Choices
    jsonSpec @Choices

    -- What hopinion.yaml accepts, drawn from the codec that accepts it. A
    -- golden because the file is what a repository writes by hand: changing
    -- what it may say is a decision, and this is where it is seen being made.
    it "has the schema the golden says" $
      goldenTextFile
        (toFilePath (resourceDir </> [relfile|schema.txt|]))
        (pure (renderPlainSchemaViaCodec @Choices))

  describe "parseChoices" $ do
    -- The whole file goes through the codec, and this is what holds it to that.
    -- Everything `parseChoices` does around the codec is the unknown-key check,
    -- so anything the codec writes must survive it. A field added to the codec
    -- and not to whatever that check reads is a setting hopinion accepts and
    -- then refuses, which fails here rather than on somebody's repository.
    it "reads back whatever the codec writes" $
      forAllValid $ \choices ->
        parseChoices (Yaml.encode choices) `shouldBe` Right choices

    -- What an empty file means is what the codec says an empty mapping means,
    -- rather than an answer written out beside it.
    it "reads a file holding nothing as the codec reads a file holding no settings" $
      parseChoices "" `shouldBe` parseChoices "{}\n"

    it "reads the rules a repository has decided against" $
      parseChoices "disabled-rules:\n  - CommentBareTodo\n"
        `shouldBe` Right (Choices {choicesDisabled = [RuleId "CommentBareTodo"]})

    it "reads a file holding nothing as nothing decided" $
      parseChoices "" `shouldBe` Right noChoices

    it "reads a file holding only comments as nothing decided" $
      parseChoices "# nothing yet\n" `shouldBe` Right noChoices

    it "reads an empty list as nothing decided" $
      parseChoices "disabled-rules: []\n" `shouldBe` Right noChoices

    -- A key nobody has heard of is a rule the writer believes is off and that
    -- every run still makes, which is what a name that is not a rule id would be,
    -- one line up. The keys it does know are the codec's, so this message names
    -- them without anybody writing them down twice.
    it "refuses a key it does not know, rather than ignoring it" $
      parseChoices "disabled_rules:\n  - CommentBareTodo\n"
        `shouldBe` Left (SettingsNobodyKnows ("disabled_rules" :| []))

    -- The sentence a reader is shown, which is the renderer's and names the file
    -- the caller opened.
    it "says which file and which setting" $
      renderChoicesFileError
        (ChoicesFileRefused [absfile|/repo/hopinion.yaml|] (SettingsNobodyKnows ("disabled_rules" :| [])))
        `shouldBe` "/repo/hopinion.yaml sets disabled_rules, which mean nothing here. A setting this \
                   \file does not know is a decision nothing acts on, so it is refused rather \
                   \than ignored. It knows: disabled-rules"

    it "refuses something that is not a set of settings" $
      parseChoices "- CommentBareTodo\n" `shouldBe` Left NotASetOfSettings

    it "refuses a name that is not a rule id" $
      parseChoices "disabled-rules:\n  - not an id\n"
        `shouldSatisfy` either (const True) (const False)
