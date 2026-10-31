{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hopinion.Check.Test.NoDummyValues.Fact (TestLiteral (..)) where

import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import Hopinion.Facts.Place

-- | A literal written in the body of a test.
data TestLiteral = TestLiteral
  { -- | The literal as the parser prints it back, so that a finding can say
    -- which of several on one line it is about.
    testLiteralText :: !Text,
    testLiteralSpan :: !Span,
    testLiteralScope :: !ScopeKey
  }
  deriving stock (Show, Eq, Generic)

instance Validity TestLiteral
