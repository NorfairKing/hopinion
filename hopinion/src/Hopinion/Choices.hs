{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | What a repository has decided about the rules it is held to.
--
-- One file at the repository root, read by one parser. The development loop is
-- handed the repository and finds it; the derivations are handed a package each
-- and are told where it is. A file rather than a flag, because a rule turned off
-- in a flake and not in the working tree is a check whose cheapest feedback
-- loop disagrees with its slowest.
--
-- Deliberately small: which packages exist and which package set builds them
-- are things the Nix layer knows, and a second copy here would be a second copy
-- to disagree.
--
-- Read here rather than through @opt-env-conf@'s own configuration support,
-- which is why 'Hopinion.OptParse' says @withoutConfig@: where the file is
-- depends on a positional argument, which cannot be resolved before the parser
-- has run.
module Hopinion.Choices
  ( Choices (..),
    noChoices,
    choicesFile,
    ChoicesError (..),
    ChoicesFileError (..),
    renderChoicesFileError,
    parseChoices,
    readChoicesFrom,
    readChoicesIn,
  )
where

import Autodocodec
import Autodocodec.Schema (ObjectSchema (..), jsonObjectSchemaViaCodec)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as JSON
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import Hopinion.Rule.Id (RuleId)
import Path (Abs, Dir, File, Path, Rel, relfile, toFilePath, (</>))
import Path.IO (forgivingAbsence)

-- | Everything the file says, which is one thing so far.
newtype Choices = Choices
  { choicesDisabled :: [RuleId]
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Choices)

instance Validity Choices

-- | Optional with an empty default, so a file that exists and says nothing is
-- the same as no file. A repository that writes this to say something else
-- later should not have to restate that it wants every rule.
instance HasObjectCodec Choices where
  objectCodec =
    Choices
      <$> optionalFieldWithOmittedDefault
        "disabled-rules"
        []
        "the rules this repository has decided against, which no run of it makes"
        .= choicesDisabled

instance HasCodec Choices where
  codec = object "Choices" objectCodec

-- | The keys this file may hold, read off the codec rather than written down
-- beside it.
--
-- A second list would be a second answer: a field added to the codec and not to
-- the list is a setting the codec accepts and 'parseChoices' refuses, which is
-- the same silence in the other direction.
knownKeys :: [Text]
knownKeys = keysOf (jsonObjectSchemaViaCodec @Choices)
  where
    keysOf :: ObjectSchema -> [Text]
    keysOf = \case
      ObjectKeySchema key _ _ _ -> [key]
      ObjectAnySchema -> []
      ObjectAnyOfSchema ss -> concatMap keysOf (NE.toList ss)
      ObjectOneOfSchema ss -> concatMap keysOf (NE.toList ss)
      ObjectAllOfSchema ss -> concatMap keysOf (NE.toList ss)

noChoices :: Choices
noChoices = Choices {choicesDisabled = []}

choicesFile :: Path Rel File
choicesFile = [relfile|hopinion.yaml|]

-- | What can be wrong with this file, as what happened rather than as the
-- sentence about it.
--
-- Every way of getting the file wrong is an error rather than a file quietly
-- ignored, because each reads as a rule turned off and behaves as a rule still
-- running. The refusals are not all here: a name that is not a rule id is
-- refused by the codec and a rule nothing answers to by the rule set.
--
-- YAML's and aeson's own messages are passed through rather than owned here.
data ChoicesError
  = NotYaml !Text
  | NotASetOfSettings
  | SettingsNobodyKnows !(NonEmpty Text)
  | SettingRefused !Text
  deriving (Show, Eq)

-- | Absence is only an error for a caller that named the file: a repository that
-- has not written one has decided nothing, which is 'readChoicesIn' below.
data ChoicesFileError
  = ChoicesFileAbsent !(Path Abs File)
  | ChoicesFileRefused !(Path Abs File) !ChoicesError
  deriving (Show, Eq)

renderChoicesFileError :: ChoicesFileError -> Text
renderChoicesFileError = \case
  ChoicesFileAbsent path ->
    T.pack
      ( unwords
          [ "There is no file at",
            concat [toFilePath path, ", and it was named as this repository's"],
            toFilePath choicesFile
          ]
      )
  ChoicesFileRefused path err -> T.pack (unwords [toFilePath path, said err])
  where
    said = \case
      NotYaml t -> unwords ["is not YAML:", T.unpack t]
      NotASetOfSettings -> "holds something that is not a set of settings."
      SettingsNobodyKnows unknown ->
        unwords
          [ "sets",
            concat [T.unpack (T.intercalate ", " (NE.toList unknown)), ","],
            "which mean nothing here. A setting this file does not know is a decision",
            "nothing acts on, so it is refused rather than ignored. It knows:",
            T.unpack (T.intercalate ", " knownKeys)
          ]
      SettingRefused t -> T.unpack t

parseChoices :: BS.ByteString -> Either ChoicesError Choices
parseChoices contents = do
  value <-
    first
      (NotYaml . T.pack . Yaml.prettyPrintParseException)
      (Yaml.decodeEither' contents)
  case value of
    -- An empty file is a file that sets nothing, which the codec already has an
    -- answer for. Handed on rather than answered here, so that what an empty
    -- file means and what an empty object means cannot come apart.
    JSON.Null -> viaCodec (JSON.Object mempty)
    JSON.Object o -> do
      assertOnlyKnownKeys (map Key.toText (KeyMap.keys o))
      viaCodec value
    _ -> Left NotASetOfSettings
  where
    -- Everything this file means, the codec means. Nothing here reads a key or
    -- a value for itself: the check above is over the key names alone, and they
    -- come from the codec too.
    viaCodec =
      first (SettingRefused . T.pack . unwords . words)
        . JSON.parseEither parseJSONViaCodec

    assertOnlyKnownKeys keys = case filter (`notElem` knownKeys) keys of
      [] -> Right ()
      unknown -> Left (SettingsNobodyKnows (NE.fromList unknown))

-- | The file at this path, which has to be there.
--
-- Named rather than found, so a name that is not a file is a mistake in what
-- named it rather than a repository that decided nothing.
readChoicesFrom :: Path Abs File -> IO (Either ChoicesFileError Choices)
readChoicesFrom path = do
  read' <- forgivingAbsence (BS.readFile (toFilePath path))
  pure $ case read' of
    Nothing -> Left (ChoicesFileAbsent path)
    Just contents -> first (ChoicesFileRefused path) (parseChoices contents)

-- | The file beside a repository, which is what the development loop reads.
--
-- Absence is a real answer here, where it is an error above: most repositories
-- want every rule, and having to write a file to say so would be a file that
-- says nothing.
readChoicesIn :: Path Abs Dir -> IO (Either ChoicesFileError Choices)
readChoicesIn root = do
  let path = root </> choicesFile
  read' <- forgivingAbsence (BS.readFile (toFilePath path))
  pure $ case read' of
    Nothing -> Right noChoices
    Just contents -> first (ChoicesFileRefused path) (parseChoices contents)
