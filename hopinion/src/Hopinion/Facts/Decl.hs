{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | What a module declares, as far as anything that is not a type checker can
-- tell: a name, a kind and a span.
--
-- Its own module, beside 'Hopinion.Facts.Name' and 'Hopinion.Facts.Place' and
-- re-exported with them, because comment attachment needs it and comments are
-- where the fact types that have an owner now live. A leaf keeps that from
-- being a cycle.
module Hopinion.Facts.Decl
  ( DeclKind (..),
    DeclFact (..),
  )
where

import Data.Validity
import GHC.Generics (Generic)
import Hopinion.Facts.Name
import Hopinion.Facts.Place

data DeclKind
  = DeclKindValue
  | DeclKindSignature
  | DeclKindData
  | DeclKindNewtype
  | DeclKindTypeSynonym
  | DeclKindClass
  | DeclKindInstance
  | DeclKindPattern
  | DeclKindForeign
  | -- | An annotation pragma. Its own kind rather than 'DeclKindOther', because
    -- it is the one declaration that is not a subject: it says something about
    -- another declaration, so a comment above it is about that other one, and
    -- a rule asking for declarations of some kind must never be handed this.
    DeclKindAnnotation
  | DeclKindOther
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance Validity DeclKind

data DeclFact = DeclFact
  { declFactName :: !DeclName,
    declFactKind :: !DeclKind,
    declFactSpan :: !Span
  }
  deriving stock (Show, Eq, Generic)

instance Validity DeclFact
