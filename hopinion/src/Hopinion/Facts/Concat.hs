{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hopinion.Facts.Concat
  ( ConcatOperand (..),
    ConcatChain (..),
  )
where

import Data.Validity
import GHC.Generics (Generic)
import Hopinion.Facts.Place

-- | What the source says an operand of a concatenation is.
--
-- Two answers because a parser has two: a string literal is one outright, and
-- everything else is a name or a call whose type is somewhere the parser cannot
-- follow. The types tier is what turns the second answer into a real one.
data ConcatOperand
  = OperandStringLiteral
  | OperandSomethingElse
  deriving stock (Show, Eq, Generic)

instance Validity ConcatOperand

-- | A chain of @<>@ and @++@ with the nesting flattened away, so that
-- @a <> b <> c@ is three operands and one fact rather than two facts sharing an
-- operand.
--
-- Both operators in one chain because they concatenate the same things and the
-- fix is the same list either way, so @a ++ b <> c@ is one chain of three.
--
-- One fact per chain because a chain is one thing to fix and therefore one
-- thing to suppress: a finding per operator would put two of them inside one
-- statement, where the second suppression a reader wrote would be one that
-- answers for nothing.
data ConcatChain = ConcatChain
  { concatChainOperands :: ![ConcatOperand],
    concatChainSpan :: !Span,
    concatChainScope :: !ScopeKey
  }
  deriving stock (Show, Eq, Generic)

instance Validity ConcatChain
