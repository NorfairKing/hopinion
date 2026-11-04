module Main (main) where

import Test.QuickCheck (property)
import Test.Syd
import Thing (double)

main :: IO ()
main = sydTest $ do
  it "doubles" (double 2 `shouldBe` 4)
  it "is even" (property (\n -> even (double n)))
