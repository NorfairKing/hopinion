module Bad where

-- The type in a signature of this module's own.
readFrom :: FilePath -> IO String
readFrom = readFile

-- And inside a list, where it is just as much a String.
resourceDirs :: [FilePath]
resourceDirs = ["test_resources"]
