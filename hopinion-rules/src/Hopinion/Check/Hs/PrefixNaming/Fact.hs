{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Hs.PrefixNaming.Fact
  ( NameRole (..),
    UnprefixedName (..),
    nameIsRight,
    newtypeFieldName,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import GHC.Generics (Generic)
import Hopinion.Facts.Name
import Hopinion.Facts.Place

-- | Which of the names a data declaration introduces this is.
--
-- Three rather than one because what each is held to differs: a constructor
-- takes the type's name as it is written, a field takes it with the first
-- letter lowered, and the one field of a newtype takes a single spelling of
-- its own.
data NameRole
  = NameRoleConstructor
  | NameRoleField
  | NameRoleNewtypeField
  deriving stock (Show, Eq, Generic)

instance Validity NameRole

-- | One constructor or field whose name is not what its role asks for.
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

-- | A name that is already right is not one of these, and a value saying
-- otherwise would be the rule's own question answered twice.
instance Validity UnprefixedName where
  validate un =
    mconcat
      [ genericValidate un,
        declare
          "the name is not what its role asks for"
          ( not
              ( nameIsRight
                  (unprefixedNameRole un)
                  (unprefixedNameType un)
                  (unprefixedNameWritten un)
              )
          )
      ]

-- | Whether a name of this role, belonging to this type, is what it should be.
nameIsRight :: NameRole -> TypeHead -> Text -> Bool
nameIsRight = \case
  NameRoleConstructor -> prefixedByType
  NameRoleField -> prefixedByType
  NameRoleNewtypeField -> \th written -> written == newtypeFieldName th

-- | Whether this name starts with the name of the type it belongs to.
--
-- Case is ignored, which is what lets one predicate answer for a constructor
-- and for a field. A field lowers the first letter of the type's name, and a
-- type whose name opens with an acronym has more than the first letter lowered
-- by anyone writing the field by hand, so the case of the prefix is not
-- something to hold a name to. What is held to is that the whole of the name is
-- there: an abbreviation of it is what this rule exists to refuse.
prefixedByType :: TypeHead -> Text -> Bool
prefixedByType th written = T.isPrefixOf (T.toLower (unTypeHead th)) (T.toLower written)

-- | What the one field of a newtype is called.
--
-- A prefix on a field says which type it belongs to, which is worth saying
-- because the fields of a data type have to be told apart from one another. A
-- newtype has exactly one field and so has nothing to tell it apart from, and
-- the prefix would only repeat what the declaration already says. The name
-- says the useful thing instead, that applying it undoes the wrapper, and it
-- says it in the one spelling the whole ecosystem already uses, so this is
-- held to exactly rather than to a prefix.
newtypeFieldName :: TypeHead -> Text
newtypeFieldName th = T.concat ["un", unTypeHead th]
