module Fixture where

data Encoder = Encoder

data Decoder = Decoder

-- | Keep 'Encoder' and 'Decoder' in sync.
encode :: Int -> Int
encode = id

-- The row 'Writer' writes and the one 'Reader' expects change together.
writeRow :: Int -> Int
writeRow = id
