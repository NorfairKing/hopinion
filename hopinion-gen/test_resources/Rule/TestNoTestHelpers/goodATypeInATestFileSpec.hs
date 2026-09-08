module GoodATypeInATestFileSpec (spec) where

import Test.Syd

-- A type is not a helper function, and a test file that needs one to say what
-- it asserts is having a different conversation.
data Colour
  = Red
  | Green
  deriving (Show, Eq)

spec :: Spec
spec = it "is not green" (Red `shouldNotBe` Green)
