{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Hs.NoAnonymousScalar.Fact
  ( AnonymousScalars (..),
    isPrimitiveTypeName,
    isAnonymousContainerName,
  )
where

import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import Hopinion.Facts.Place

-- | One positional constructor holding an anonymous primitive its own name
-- cannot speak for.
--
-- The span covers the whole constructor declaration, because both fixes
-- rewrite the whole of it: a newtype on each of the primitives, or record
-- syntax over the lot.
data AnonymousScalars = AnonymousScalars
  { anonymousScalarsConstructor :: !Text,
    -- | How many fields the constructor holds, which is why the primitives
    -- below are anonymous: a name speaks for one payload, and there is more
    -- than one here for it to speak for.
    anonymousScalarsFields :: !Word,
    -- | The primitives it holds, in the order they are written and under the
    -- names a reader meets them by, which is what tells two findings in one
    -- declaration apart.
    anonymousScalarsTypes :: ![Text],
    anonymousScalarsSpan :: !Span,
    anonymousScalarsScope :: !ScopeKey
  }
  deriving stock (Show, Eq, Generic)

-- | A constructor of one field is named by its own name, so a fact recording
-- one would be the rule without its exception wearing a fact's clothes. Both
-- halves, since a row saying nothing is anonymous says nothing at all.
instance Validity AnonymousScalars where
  validate as =
    mconcat
      [ genericValidate as,
        declare
          "the constructor holds more than one field"
          (anonymousScalarsFields as > 1),
        declare
          "the constructor holds an anonymous primitive"
          (not (null (anonymousScalarsTypes as)))
      ]

-- | Whether a type written under this name, and applied to nothing, is one of
-- the primitives a constructor's own name has to speak for.
--
-- The name alone, with whatever qualifier it was written under already off,
-- because the module a type arrived through says nothing about what a field
-- holding it holds: @T.Text@ and the @Text@ beside it are the same scalar to
-- whoever is counting arguments at a call site. A synonym is not resolved for
-- the same reason in reverse: it is a name, and what is wrong with a synonym
-- standing in for a newtype is HsNewtypeNotSynonym's complaint rather than
-- this one.
--
-- Bool is deliberately not here. Two of them in one constructor is
-- HsNoDomainBool's finding, and the same line reported twice in two
-- vocabularies is worse than reported once.
isPrimitiveTypeName :: Text -> Bool
isPrimitiveTypeName name =
  name
    `elem` [ "Text",
             "String",
             "ByteString",
             "Char",
             "Int",
             "Int8",
             "Int16",
             "Int32",
             "Int64",
             "Integer",
             "Word",
             "Word8",
             "Word16",
             "Word32",
             "Word64",
             "Double",
             "Float",
             "Rational"
           ]

-- | Whether a type of one argument hands a reader of it exactly what a reader
-- of its argument would have had.
--
-- @Maybe@ holds at most one of the thing, so a field holding one is as
-- anonymous as a field holding the thing itself, and the newtype that fixes it
-- goes inside: a @Maybe Text@ whose @Text@ is a @Username@ is a
-- @Maybe Username@.
--
-- A list and a @NonEmpty@ are not here. A collection of a thing has a
-- different character from the thing, and a reader of one has been told that
-- there are several, which is something. What this costs is a field holding
-- the only primitive in its constructor as a list, which is a shape this rule
-- leaves alone rather than one it reads.
--
-- A type that says something of its own is not here either, however few
-- arguments it takes: a reader of @Path Abs File@ has been told what it is.
isAnonymousContainerName :: Text -> Bool
isAnonymousContainerName name = name == "Maybe"
