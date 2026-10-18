{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hopinion.Check.Hs.NoAnonymousScalar.Fact (AnonymousScalars (..)) where

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
