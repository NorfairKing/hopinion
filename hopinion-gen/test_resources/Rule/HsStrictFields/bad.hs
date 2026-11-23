{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Bad where

data Point = Point
  { pointX :: Int,
    pointY :: !Int
  }

data Pair = Pair Int !Int

data Name = Name
  { nameFirst, nameLast :: String
  }

data Expr where
  ExprLit :: Int -> Expr

data family Keyed k

data instance Keyed Int = KeyedInt Int
