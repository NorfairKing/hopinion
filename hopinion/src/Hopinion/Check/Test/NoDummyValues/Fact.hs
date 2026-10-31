{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hopinion.Check.Test.NoDummyValues.Fact (TestLiteral (..)) where

import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import Hopinion.Facts.Place

-- | A literal a test runs on: written in its body, or in a binding it uses.
data TestLiteral = TestLiteral
  { -- | The literal as the parser prints it back, so that a finding can say
    -- which of several on one line it is about.
    testLiteralText :: !Text,
    testLiteralSpan :: !Span,
    testLiteralScope :: !ScopeKey,
    -- | The test it is in, or the binding outside every test that it is in.
    -- Literals are reported one owner at a time, so that one suppression above
    -- a test answers for everything made up in it.
    testLiteralOwner :: !Span
  }
  deriving stock (Show, Eq, Generic)

instance Validity TestLiteral
