{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Test.Syd (sydTest)
import Test.Syd.Validity (genValidSpec)
import Thing

main :: IO ()
main = sydTest (genValidSpec @A)
