{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Rule identity, separated from the rules themselves so that facts and
-- annotations can name a rule without depending on any check.
module Hopinion.Rule.Id
  ( RuleId (..),
    parseRuleId,
    Level (..),
    levelText,
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAlphaNum, isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Text ()
import GHC.Generics (Generic)

-- | What a rule is called.
--
-- Open, and that is the point. A repository runs this tool's rules plus rules
-- of its own, so what makes an id real cannot be that this module listed it. It
-- is that some rule in the set being run answers to it, which
-- 'Hopinion.Rule.useOf' is the question for.
--
-- The shape is closed, which is what keeps it a name rather than a string, and
-- 'Validity' is what says the shape.
newtype RuleId = RuleId {ruleIdText :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec RuleId)

-- | A suppression is @[allow:RuleId] reason@, and the bracket is meant to be
-- able to grow a second colon-separated field later, which stays possible only
-- while no id holds a colon of its own.
instance Validity RuleId where
  validate rid@(RuleId t) =
    mconcat
      [ genericValidate rid,
        declare "the id is not empty" (not (T.null t)),
        declare "the id starts with an upper-case letter" (maybe False (isUpper . fst) (T.uncons t)),
        declare
          "the rest of the id is alphanumeric or an underscore"
          (T.all (\c -> isAlphaNum c || c == '_') (T.drop 1 t))
      ]

-- | Syntax only. Whether a rule of this name exists is a different question
-- with a different answer for every repository, and 'Hopinion.Rule.useOf' is
-- where it is asked.
parseRuleId :: Text -> Maybe RuleId
parseRuleId t = let rid = RuleId t in if isValid rid then Just rid else Nothing

instance HasCodec RuleId where
  codec =
    named "RuleId" $
      bimapCodec
        (maybe (Left "not a rule id") Right . parseRuleId)
        ruleIdText
        codec

data Level
  = LevelModule
  | LevelPackage
  | LevelProject
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance Validity Level

levelText :: Level -> Text
levelText = \case
  LevelModule -> "module"
  LevelPackage -> "package"
  LevelProject -> "project"
