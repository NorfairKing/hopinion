-- | Enough of tasty's interface for a suite written against it to compile.
module Test.Tasty (TestTree, defaultMain, testGroup) where

type TestTree = IO ()

defaultMain :: TestTree -> IO ()
defaultMain t = t

testGroup :: String -> [TestTree] -> TestTree
testGroup _ = sequence_
