{-# LANGUAGE DerivingVia #-}

module Thing where

-- An ordinary instance declaration.
data Plain = Plain

instance GenValid Plain

-- A standalone deriving declaration.
data Standalone = Standalone

deriving instance GenValid Standalone

-- Every deriving clause flavour.
data Stocked = Stocked
  deriving stock (GenValid)

newtype Wrapped = Wrapped Int
  deriving newtype (GenValid)

data AnyOf = AnyOf
  deriving anyclass (GenValid)

-- The subject is the enclosing type, not the representation in the via clause.
data ViaCodec = ViaCodec
  deriving (GenValid) via (Autodocodec ViaCodec)

-- A parameterised type, satisfied by an application at any instantiation.
data Allowed a = Allowed a

instance GenValid (Allowed a)
