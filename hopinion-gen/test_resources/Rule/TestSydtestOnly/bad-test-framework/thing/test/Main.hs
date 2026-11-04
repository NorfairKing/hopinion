module Main (main) where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Thing (double)

main :: IO ()
main = defaultMain [testGroup "thing" [testCase "doubles" (double 2 @?= 4)]]
