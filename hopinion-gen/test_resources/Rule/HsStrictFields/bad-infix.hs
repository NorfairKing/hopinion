module BadInfix where

data Op
  = OpNeg !Int
  | Int :+ !Int
