{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hopinion.Check.Hs.PrefixNaming.Fact
  ( NameRole (..),
    UnprefixedName (..),
    prefixedByType,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import GHC.Generics (Generic)
import Hopinion.Facts.Name
import Hopinion.Facts.Place

-- | Which of the two names a data declaration introduces this is.
--
-- Two rather than one because the fix reads differently in each: a constructor
-- takes the type's name as it is written, and a field takes it with the first
-- letter lowered.
data NameRole
  = NameRoleConstructor
  | NameRoleField
  deriving stock (Show, Eq, Generic)

instance Validity NameRole

-- | One constructor or field whose name does not start with the name of the
-- type it belongs to.
--
-- The span is the name alone, because the name alone is what changes: the rest
-- of the declaration is the same before and after.
data UnprefixedName = UnprefixedName
  { unprefixedNameRole :: !NameRole,
    unprefixedNameWritten :: !Text,
    unprefixedNameType :: !TypeHead,
    unprefixedNameSpan :: !Span,
    unprefixedNameScope :: !ScopeKey
  }
  deriving stock (Show, Eq, Generic)

-- | A name that does carry its type's is not one of these, and a value saying
-- otherwise would be the rule's own question answered twice.
instance Validity UnprefixedName where
  validate un =
    mconcat
      [ genericValidate un,
        declare
          "the name does not start with the name of its type"
          (not (prefixedByType (unprefixedNameType un) (unprefixedNameWritten un)))
      ]

-- | Whether this name starts with the name of the type it belongs to.
--
-- Case is ignored, which is what lets one predicate answer for a constructor
-- and for a field. A field lowers the first letter of the type's name, and a
-- type whose name opens with an acronym has more than the first letter lowered
-- by anyone writing the field by hand, so the case of the prefix is not
-- something to hold a name to. What is held to is that the whole of the name is
-- there: an abbreviation of it is what this rule exists to refuse.
prefixedByType :: TypeHead -> Text -> Bool
prefixedByType th written = T.isPrefixOf (T.toLower (typeHeadText th)) (T.toLower written)
