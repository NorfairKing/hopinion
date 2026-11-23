{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Good where

import Data.Map (Map)

data Point = Point
  { pointX :: !Int,
    pointY :: !Int
  }

data Pair = Pair !Int !Int

data Shape
  = ShapeCircle !Double
  | ShapeRectangle {-# UNPACK #-} !Double !Double

data Memo = Memo
  { memoKey :: !Int,
    -- [allow:HsStrictFields] A memo nobody reads never computes its table.
    memoTable :: Map Int Int
  }

-- [allow:HsStrictFields] A stream can be infinite.
data Stream = Stream !Int Stream

data Expr where
  ExprLit :: !Int -> Expr
  ExprAdd :: !Expr -> !Expr -> Expr

data family Keyed k

data instance Keyed Int = KeyedInt !Int

newtype Wrapper = Wrapper {unWrapper :: Int}
