module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Test.Tasty.QuickCheck as QC
import Thing (double)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "thing"
        [ testCase "doubles" (double 2 @?= 4),
          QC.testProperty "is even" (\n -> even (double n))
        ]
    )
