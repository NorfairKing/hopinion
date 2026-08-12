{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Thing

main :: IO ()
main = genValidSpec @A
