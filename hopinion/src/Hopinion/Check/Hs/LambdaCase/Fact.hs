{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hopinion.Check.Hs.LambdaCase.Fact
  ( ArgumentShape (..),
    CasedArgument (..),
  )
where

import Data.Validity
import GHC.Generics (Generic)
import Hopinion.Facts.Place

-- | The two ways a function can name an argument in order to take it apart.
--
-- Two rather than one because the fix reads differently in each: one has a
-- @case@ to delete along with the argument, and the other has equations to
-- merge.
data ArgumentShape
  = ArgumentNamedThenCased
  | ArgumentSplitOverEquations
  deriving stock (Show, Eq, Generic)

instance Validity ArgumentShape

-- | One binding whose last argument exists only to be taken apart.
--
-- The span covers every equation, because the whole binding is what changes and
-- therefore what a suppression answers for.
data CasedArgument = CasedArgument
  { casedArgumentShape :: !ArgumentShape,
    casedArgumentSpan :: !Span,
    casedArgumentScope :: !ScopeKey
  }
  deriving stock (Show, Eq, Generic)

instance Validity CasedArgument
