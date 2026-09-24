module Fixture where

data Encoder = Encoder

data Decoder = Decoder

-- | [tag:WireFormat] Keep 'Encoder' and 'Decoder' in sync.
encode :: Int -> Int
encode = id

-- | [ref:WireFormat]
decode :: Int -> Int
decode = id

data Reader = Reader

data Writer = Writer

-- [check:tag TableShape] 'Reader' and 'Writer' change together.
readRow :: Int -> Int
readRow = id

-- [check:ref TableShape]
writeRow :: Int -> Int
writeRow = id

-- Returns once the 'Replica' is in sync, which is a state this waits for
-- rather than a coupling anybody has to maintain.
await :: Int -> Int
await = id

-- Keep this one simple. Two 'Replica' rows in lockstep would be a state the
-- reader happens to observe, and nothing here asks for it.
simple :: Int -> Int
simple = id

-- Keep the encoder and the decoder in sync. Prose alone names no other place,
-- so there is nothing a tag could be written against.
unnamed :: Int -> Int
unnamed = id

-- | One entry of a 'BranchSet', which describes this in terms of that and
-- couples nothing.
entry :: Int -> Int
entry = id
