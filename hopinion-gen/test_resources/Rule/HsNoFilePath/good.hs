module Good where

import Path (Abs, Dir, File, Path, Rel, toFilePath)

-- A path that says what it is, and a conversion at the edge, which is what the
-- rule deliberately stays quiet about.
readFrom :: Path Abs File -> IO String
readFrom file = readFile (toFilePath file)

resourcesIn :: Path b Dir -> Path b Dir
resourcesIn = id

named :: Path Rel File -> String
named = toFilePath

-- The word FilePath in a comment is not a use of the type, and neither is the
-- word inside a string. Both are single tokens of their own, which is why this
-- is read off the token stream.
explains :: String
explains = "a FilePath is a String"
