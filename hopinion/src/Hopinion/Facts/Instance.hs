{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hopinion.Facts.Instance
  ( InstanceMethods (..),
    InstanceOrigin (..),
    InstanceFact (..),
  )
where

import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)
import Hopinion.Facts.Name
import Hopinion.Facts.Place

-- | What the methods an instance declaration writes out do with what they are
-- handed.
--
-- Carried by the origin rather than beside it, because a derived instance has
-- no body and a field answering for one would be answering about nothing.
data InstanceMethods
  = MethodsUseArguments
  | -- | Every method the instance writes out discards all of its arguments, so
    -- what it produces cannot depend on the value it was given. An instance
    -- that writes out no methods at all is not this: what its class defaults do
    -- with the value is not in the file.
    MethodsIgnoreArguments
  deriving stock (Show, Eq, Ord, Generic)

instance Validity InstanceMethods

-- | A closed sum on purpose: a further form forces every site to be
-- reconsidered rather than falling into a catch-all, and a missed form is a
-- silently unsatisfied obligation.
--
-- 'OriginDerivingUnspecified' is a deriving clause with no strategy. Which one
-- GHC picks depends on the extensions in force, so recording it as stock would
-- be a guess.
data InstanceOrigin
  = OriginInstanceDecl !InstanceMethods
  | OriginStandaloneDeriving
  | OriginDerivingStock
  | OriginDerivingNewtype
  | OriginDerivingAnyclass
  | OriginDerivingVia !TypeHead
  | OriginDerivingUnspecified
  deriving stock (Show, Eq, Ord, Generic)

instance Validity InstanceOrigin

data InstanceFact = InstanceFact
  { instanceFactClass :: !Text,
    instanceFactType :: !TypeHead,
    instanceFactOrigin :: !InstanceOrigin,
    instanceFactSpan :: !Span,
    -- | Where an annotation about this instance has to go, which is also where
    -- the finding is reported.
    instanceFactScope :: !ScopeKey
  }
  deriving stock (Show, Eq, Generic)

instance Validity InstanceFact
