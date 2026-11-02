{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What the obligation family reads out of a module's tokens, beside the
-- family because nothing else reads it.
module Hopinion.Check.Obligation.Fact
  ( TypeAppFact (..),
    TemplateHaskellUse (..),
    templateHaskellUseText,
    parseTemplateHaskellUse,
  )
where

import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import Database.Persist (PersistField (..))
import Database.Persist.Sql (PersistFieldSql (..), SqlType (..))
import GHC.Generics (Generic)
import Hopinion.Facts.Name
import Hopinion.Facts.Place

-- | @f \@T@ or @f \@(T a b)@: the function's name and the head type constructor,
-- with applied arguments dropped.
data TypeAppFact = TypeAppFact
  { typeAppFactFunction :: !Text,
    typeAppFactHead :: !TypeHead,
    typeAppFactSpan :: !Span
  }
  deriving stock (Show, Eq, Generic)

instance Validity TypeAppFact

-- | A parser cannot see what Template Haskell generates, so a module that uses
-- it records the fact, and the rules that care put the module to the compiler
-- rather than concluding that what they cannot see is not there.
--
-- Which of the two forms it is, because they hide different amounts: a splice
-- runs arbitrary code and what comes back appears nowhere in the file, where a
-- quasiquote expands one body that is in the file. A module doing both records
-- the splice, which is the weaker guarantee.
data TemplateHaskellUse
  = TemplateHaskellUseNone
  | TemplateHaskellUseQuasiQuotes
  | TemplateHaskellUseSplices
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance Validity TemplateHaskellUse

instance PersistField TemplateHaskellUse where
  toPersistValue = toPersistValue . templateHaskellUseText
  fromPersistValue v = do
    t <- fromPersistValue v
    maybe (Left "not a Template Haskell use") Right (parseTemplateHaskellUse t)

instance PersistFieldSql TemplateHaskellUse where
  sqlType _ = SqlString

templateHaskellUseText :: TemplateHaskellUse -> Text
templateHaskellUseText = \case
  TemplateHaskellUseNone -> "no"
  TemplateHaskellUseQuasiQuotes -> "quasiquotes"
  TemplateHaskellUseSplices -> "splices"

parseTemplateHaskellUse :: Text -> Maybe TemplateHaskellUse
parseTemplateHaskellUse t =
  lookup t [(templateHaskellUseText u, u) | u <- [minBound .. maxBound]]
