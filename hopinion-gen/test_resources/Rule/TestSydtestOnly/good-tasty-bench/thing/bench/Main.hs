module Main (main) where

import Test.Tasty.Bench (bench, defaultMain, whnf)
import Thing (double)

main :: IO ()
main = defaultMain [bench "double" (whnf double 2)]
