{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hopinion.Facts.TypeApp (TypeAppFact (..)) where

import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)
import Hopinion.Facts.Name
import Hopinion.Facts.Place

data TypeAppFact = TypeAppFact
  { typeAppFactFunction :: !Text,
    typeAppFactHead :: !TypeHead,
    typeAppFactSpan :: !Span
  }
  deriving stock (Show, Eq, Generic)

instance Validity TypeAppFact
