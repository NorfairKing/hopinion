module Main (main) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Thing (double)

main :: IO ()
main = hspec $ do
  it "doubles" (double 2 `shouldBe` 4)
  prop "is even" (\n -> even (double n))
