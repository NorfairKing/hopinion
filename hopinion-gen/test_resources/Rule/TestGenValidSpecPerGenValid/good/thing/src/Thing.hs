{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Thing where

import Data.GenValidity (GenValid)
import Data.Validity (Validity)
import GHC.Generics (Generic)

-- An ordinary instance declaration.
data Plain = Plain
  deriving stock (Show, Generic)

instance Validity Plain

instance GenValid Plain

-- A standalone deriving declaration.
data Standalone = Standalone
  deriving stock (Show, Generic)

instance Validity Standalone

deriving instance GenValid Standalone

-- Every deriving clause flavour that can derive the class.
data Unstrategised = Unstrategised
  deriving stock (Show, Generic)
  deriving (Validity, GenValid)

newtype Wrapped = Wrapped Int
  deriving stock (Show, Generic)
  deriving newtype (Validity, GenValid)

data AnyOf = AnyOf
  deriving stock (Show, Generic)
  deriving anyclass (Validity, GenValid)

-- The subject is the enclosing type, not the representation in the via clause.
newtype ViaInt = ViaInt Int
  deriving stock (Show, Generic)
  deriving (Validity, GenValid) via Int

-- A parameterised type, satisfied by an application at any instantiation.
data Allowed a = Allowed a
  deriving stock (Show, Generic)

instance (Validity a) => Validity (Allowed a)

instance (GenValid a) => GenValid (Allowed a)
