module Annotated where

{-# ANN value ("NOCOVER" :: FilePath) #-}

-- [allow:HsNoFilePath] The type in the annotation above is scoped to the
-- binding it annotates, which is where this suppression has to go.
value :: Int
value = 1
