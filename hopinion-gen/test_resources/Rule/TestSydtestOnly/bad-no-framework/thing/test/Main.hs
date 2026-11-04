module Main (main) where

import Thing (double)

main :: IO ()
main = if double 2 == 4 then pure () else fail "double is wrong"
