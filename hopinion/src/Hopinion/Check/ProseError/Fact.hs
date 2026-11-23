{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

-- | What a declaration that names an error says about it, in the core because
-- more than one rule reads it: the rule that asks for an error type of its own,
-- and the rule that asks for a newtype and leaves those declarations to the
-- first.
module Hopinion.Check.ProseError.Fact
  ( ErrorType (..),
    proseErrorName,
    ErrorDeclShape (..),
    ErrorDecl (..),
  )
where

import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import Hopinion.Facts.Place

-- | What the source says an error is.
--
-- Two ways to be prose rather than one, because @String@ and @Text@ are
-- different types that a reader fixes the same way and that a rule has no
-- reason to tell apart beyond naming what it found.
data ErrorType
  = ErrorTypeString
  | ErrorTypeText
  | ErrorTypeSomethingElse
  deriving stock (Show, Generic)

instance Validity ErrorType

-- | What to call an error that is prose, and nothing at all for one that is
-- already a type.
proseErrorName :: ErrorType -> Maybe String
proseErrorName = \case
  ErrorTypeString -> Just "String"
  ErrorTypeText -> Just "Text"
  ErrorTypeSomethingElse -> Nothing

-- | How a declaration that calls itself an error is written.
--
-- Two shapes because the fix differs: a synonym is deleted and replaced by a
-- type, where a wrapper already is one and needs constructors instead.
data ErrorDeclShape
  = ErrorDeclShapeSynonym
  | ErrorDeclShapeWrapper
  deriving stock (Show, Generic)

instance Validity ErrorDeclShape

-- | A declaration that names itself an error and is prose underneath.
--
-- The span is the prose rather than the declaration, so that what is underlined
-- is what has to change.
data ErrorDecl = ErrorDecl
  { errorDeclName :: !Text,
    errorDeclShape :: !ErrorDeclShape,
    errorDeclError :: !ErrorType,
    errorDeclSpan :: !Span,
    errorDeclScope :: !ScopeKey
  }
  deriving stock (Show, Generic)

instance Validity ErrorDecl
