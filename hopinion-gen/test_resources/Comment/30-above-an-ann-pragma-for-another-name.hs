module Fixture where

-- | What resolveTarget is for.
{-# ANN otherTarget ("NOCOVER" :: String) #-}
resolveTarget :: Int
resolveTarget = 1

otherTarget :: Int
otherTarget = 2
