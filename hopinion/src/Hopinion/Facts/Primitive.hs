{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The types this tool reads as primitives, and the containers that do not
-- change what a type is about.
--
-- One module rather than a list beside each rule that wants one. Two rules ask
-- about the same set from opposite directions, HsNoAnonymousScalar about a
-- field that went without a name and HsScalarNewtypeUnused about the newtype
-- that would have given it one, and a set that disagreed with itself would have
-- one rule asking for what the other does not read.
--
-- What is not here is which containers are transparent, because the two rules
-- want different answers and both are right. One asks whether a field holds an
-- anonymous primitive, and a collection of a thing is not the thing. The other
-- asks where a newtype would go, and it goes inside the collection. Each says
-- so where it reads its own types.
module Hopinion.Facts.Primitive
  ( Primitive,
    unPrimitive,
    primitiveNamed,
    isPrimitiveTypeName,
  )
where

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import Database.Persist (PersistField (..))
import Database.Persist.Sql (PersistFieldSql (..), SqlType (..))
import GHC.Generics (Generic)

-- | A type this tool reads as a primitive.
--
-- A name rather than a constructor per type, because everything done with one
-- is to compare it with another and to print it: the list below is a list of
-- library types rather than a shape anything here matches on.
--
-- Abstract on purpose. 'primitiveNamed' is the only way to make one, so a type
-- this tool says nothing about cannot become one by being written down.
newtype Primitive = Primitive {unPrimitive :: Text}
  deriving stock (Show, Eq, Generic)

instance Validity Primitive where
  validate p =
    mconcat
      [ genericValidate p,
        declare "the name is one this tool reads as a primitive" (unPrimitive p `elem` primitiveNames)
      ]

-- | Stored as the name it is and read back through 'primitiveNamed', so a row
-- naming a type this tool does not read as a primitive fails here rather than
-- inside a check.
instance PersistField Primitive where
  toPersistValue = toPersistValue . unPrimitive
  fromPersistValue v = do
    t <- fromPersistValue v
    maybe (Left "not a primitive") Right (primitiveNamed t)

instance PersistFieldSql Primitive where
  sqlType _ = SqlString

primitiveNamed :: Text -> Maybe Primitive
primitiveNamed t = if t `elem` primitiveNames then Just (Primitive t) else Nothing

-- | Whether a type written under this name, and applied to nothing, is one of
-- them.
--
-- The name alone, with whatever qualifier it was written under already off,
-- because the module a type arrived through says nothing about what a field
-- holding it holds: @T.Text@ and the @Text@ beside it are the same scalar to
-- whoever is counting arguments at a call site. A synonym is not resolved for
-- the same reason in reverse: it is a name, and what is wrong with a synonym
-- standing in for a newtype is HsNewtypeNotSynonym's complaint.
isPrimitiveTypeName :: Text -> Bool
isPrimitiveTypeName = isJust . primitiveNamed

-- | The types a scalar can be written as.
--
-- @Bool@ is not among them. A domain flag written as a @Bool@ wants a type with
-- two constructors rather than a newtype around one, which is a different
-- complaint with a different fix, and it is HsNoDomainBool's to make. The same
-- line reported twice in two vocabularies is worse than reported once.
primitiveNames :: [Text]
primitiveNames =
  [ "Text",
    "String",
    "ByteString",
    "Char",
    "Int",
    "Int8",
    "Int16",
    "Int32",
    "Int64",
    "Integer",
    "Word",
    "Word8",
    "Word16",
    "Word32",
    "Word64",
    "Double",
    "Float",
    "Rational"
  ]
