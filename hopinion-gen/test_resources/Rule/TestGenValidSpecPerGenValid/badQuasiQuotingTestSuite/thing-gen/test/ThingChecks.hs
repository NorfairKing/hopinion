{-# LANGUAGE QuasiQuotes #-}

module ThingChecks (spec) where

import Path (relfile)

spec :: IO ()
spec = print [relfile|thing.txt|]
