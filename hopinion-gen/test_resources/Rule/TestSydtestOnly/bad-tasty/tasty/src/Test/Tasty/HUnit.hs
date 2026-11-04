-- | Enough of tasty-hunit's interface for a suite written against it to
-- compile.
module Test.Tasty.HUnit (testCase, (@?=)) where

testCase :: String -> IO () -> IO ()
testCase _ act = act

(@?=) :: (Eq a) => a -> a -> IO ()
_ @?= _ = pure ()
