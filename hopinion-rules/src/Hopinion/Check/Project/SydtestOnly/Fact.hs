{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hopinion.Check.Project.SydtestOnly.Fact (ImportFact (..)) where

import Data.Validity
import GHC.Generics (Generic)
import Hopinion.Facts.Name
import Hopinion.Facts.Place

-- | One module a module imports, by the name its import wrote.
data ImportFact = ImportFact
  { importFactModule :: !ModuleKey,
    importFactSpan :: !Span
  }
  deriving stock (Show, Eq, Generic)

instance Validity ImportFact
