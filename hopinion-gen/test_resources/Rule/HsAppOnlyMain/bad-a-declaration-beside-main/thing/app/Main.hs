module Main (main) where

import Thing (thing)

twice :: IO () -> IO ()
twice act = act >> act

main :: IO ()
main = twice thing
