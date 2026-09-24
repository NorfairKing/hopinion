module Fixture where

-- [check] Keep 'Encoder' and 'Decoder' in sync.
encode :: Int -> Int
encode = id

-- [check:all src/Writer/**] 'Reader' and 'Writer' change together.
readRow :: Int -> Int
readRow = id
