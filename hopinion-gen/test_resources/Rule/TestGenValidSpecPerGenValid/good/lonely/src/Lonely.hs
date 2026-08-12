module Lonely where

-- A package with no gen package is fine as long as it asks nothing of one.
-- Give this type a GenValid instance and there is nowhere for its test to go,
-- which is what the bad case next door is.
data Orphan = Orphan
