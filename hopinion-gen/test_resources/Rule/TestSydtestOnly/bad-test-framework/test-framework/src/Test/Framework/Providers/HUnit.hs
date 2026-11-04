-- | Enough of test-framework-hunit's interface for a suite written against it
-- to compile.
module Test.Framework.Providers.HUnit (testCase) where

testCase :: String -> IO () -> IO ()
testCase _ act = act
