{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hopinion.Check.Hs.NoStringError.Fact
  ( ErrorType (..),
    ErrorSlot (..),
    ErrorDeclShape (..),
    ErrorDecl (..),
  )
where

import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import Hopinion.Facts.Place

-- | What the source says the error of a slot is.
--
-- Two ways to be prose rather than one, because @String@ and @Text@ are
-- different types that a reader fixes the same way and that a rule has no
-- reason to tell apart beyond naming what it found.
data ErrorType
  = ErrorTypeString
  | ErrorTypeText
  | ErrorTypeSomethingElse
  deriving stock (Show, Eq, Generic)

instance Validity ErrorType

-- | One place a type says what a failure is: the error argument of an
-- @Either@, an @ExceptT@ or whatever else is written to carry one.
--
-- The span is the argument rather than the whole type, because the argument is
-- the part to change and the rest of the signature is right.
data ErrorSlot = ErrorSlot
  { -- | The type the slot belongs to, so a finding can say which of them this
    -- is without a reader going back to the line to find out.
    errorSlotCarrier :: !Text,
    errorSlotError :: !ErrorType,
    errorSlotSpan :: !Span,
    errorSlotScope :: !ScopeKey
  }
  deriving stock (Show, Eq, Generic)

instance Validity ErrorSlot

-- | How a declaration that calls itself an error is written.
--
-- Two shapes because the fix differs: a synonym is deleted and replaced by a
-- type, where a wrapper already is one and needs constructors instead.
data ErrorDeclShape
  = ErrorDeclSynonym
  | ErrorDeclWrapper
  deriving stock (Show, Eq, Generic)

instance Validity ErrorDeclShape

-- | A declaration that names itself an error and is prose underneath.
--
-- The span is the prose rather than the declaration, so that what is underlined
-- is what has to change, the same as for a slot.
data ErrorDecl = ErrorDecl
  { errorDeclName :: !Text,
    errorDeclShape :: !ErrorDeclShape,
    errorDeclError :: !ErrorType,
    errorDeclSpan :: !Span,
    errorDeclScope :: !ScopeKey
  }
  deriving stock (Show, Eq, Generic)

instance Validity ErrorDecl
