{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hopinion.Check.Hs.NoStringError.Fact (ErrorSlot (..)) where

import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import Hopinion.Check.ProseError.Fact (ErrorType)
import Hopinion.Facts.Place

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
  deriving stock (Show, Generic)

instance Validity ErrorSlot
