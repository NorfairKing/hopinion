module Equations where

import Data.Validity

data Shape
  = ShapeCircle !Int
  | ShapeSquare !Int

instance Validity Shape where
  -- [allow:HsValidateExtendsGeneric] A finding about a method written as
  -- several equations spans all of them, and the suppression above the first
  -- one answers for it.
  --
  -- [allow:HsLambdaCase] Written as several equations on purpose, since that
  -- is the finding the other suppression is about.
  validate (ShapeCircle r) = declare "the radius is not negative" (r >= 0)
  validate (ShapeSquare s) = declare "the side is not negative" (s >= 0)
