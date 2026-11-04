-- | Enough of test-framework's interface for a suite written against it to
-- compile.
module Test.Framework (Test, defaultMain, testGroup) where

type Test = IO ()

defaultMain :: [Test] -> IO ()
defaultMain = sequence_

testGroup :: String -> [Test] -> Test
testGroup _ = sequence_
