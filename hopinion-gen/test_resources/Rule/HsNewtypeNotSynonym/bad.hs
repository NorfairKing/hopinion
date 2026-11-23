{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Bad where

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Short (ShortByteString)
import Data.Fixed (Centi, Deci, Micro, Milli, Nano, Pico, Uni)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Short (ShortText)
import Data.UUID (UUID)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Natural (Natural)

-- An id, a name and a count, each one the type it names and nothing more.
type UserId = Text

type UserName = String

type RetryCount = Int

-- Qualified, and lazy, are the same scalars under another spelling.
type Body = LB.ByteString

type Comment = TL.Text

-- String spelled out is still String.
type Path = [Char]

-- Parentheses do not make a type compound.
type Price = (Double)

-- Nor does a kind annotation, inside the right-hand side or beside the name.
type Weight = (Double :: Type)

type Label :: Type
type Label = Text

-- A phantom parameter disappears when the synonym is expanded, so Key User and
-- Key Order are the same type and can be swapped.
type Key record = Int64

type Tagged tag = Int

-- An identifier is a scalar whatever library it comes from.
type OrderId = UUID

type Handle = ShortText

-- So is a fixed-precision number.
type Amount = Centi

type Discount = Deci

type Count = Uni

type Dose = Milli

type Delay = Micro

type Duration = Nano

type Distance = Pico

-- Every other scalar, once each.
type Initial = Char

type Digest = SB.ByteString

type Frame = LB.ByteString

type Fingerprint = StrictByteString

type Payload = LazyByteString

type Token = ShortByteString

type Summary = LText

type Note = LazyText

type Title = StrictText

type Rank = Int8

type Port = Int16

type Quantity = Int32

type Offset = Int64

type Total = Integer

type Size = Natural

type Byte = Word

type Octet = Word8

type Code = Word16

type Checksum = Word32

type Nonce = Word64

type Temperature = Float

type Share = Rational

type Measurement = Scientific

-- Named for a failure, but not prose, so the error rule says nothing about it.
type ExitError = Int

-- The qualifier does not hide the scalar.
type Slug = T.Text
