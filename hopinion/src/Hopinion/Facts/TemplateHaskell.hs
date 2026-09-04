{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Facts.TemplateHaskell
  ( TemplateHaskellUse (..),
    templateHaskellUseText,
    parseTemplateHaskellUse,
  )
where

import Data.Text (Text)
import Data.Validity
import Database.Persist (PersistField (..))
import Database.Persist.Sql (PersistFieldSql (..), SqlType (..))
import GHC.Generics (Generic)

-- | A parser cannot see what Template Haskell generates, so a module that uses
-- it records the fact, and the rules that care put the module to the compiler
-- rather than concluding that what they cannot see is not there.
--
-- Which of the two forms it is, because they hide different amounts: a splice
-- runs arbitrary code and what comes back appears nowhere in the file, where a
-- quasiquote expands one body that is in the file. A module doing both records
-- the splice, which is the weaker guarantee.
data TemplateHaskellUse
  = NoTemplateHaskell
  | UsesQuasiQuotes
  | UsesSplices
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance Validity TemplateHaskellUse

-- | Stored as the spelling it already has rather than through Show and Read, so
-- a stored row stays readable and a renamed constructor cannot silently change
-- what was written.
instance PersistField TemplateHaskellUse where
  toPersistValue = toPersistValue . templateHaskellUseText
  fromPersistValue v = do
    t <- fromPersistValue v
    maybe (Left "not a Template Haskell use") Right (parseTemplateHaskellUse t)

instance PersistFieldSql TemplateHaskellUse where
  sqlType _ = SqlString

templateHaskellUseText :: TemplateHaskellUse -> Text
templateHaskellUseText = \case
  NoTemplateHaskell -> "no"
  UsesQuasiQuotes -> "quasiquotes"
  UsesSplices -> "splices"

parseTemplateHaskellUse :: Text -> Maybe TemplateHaskellUse
parseTemplateHaskellUse t =
  lookup t [(templateHaskellUseText u, u) | u <- [minBound .. maxBound]]
