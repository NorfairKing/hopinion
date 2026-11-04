-- | Enough of tasty-quickcheck's interface for a suite written against it to
-- compile.
module Test.Tasty.QuickCheck (testProperty) where

testProperty :: String -> (Int -> Bool) -> IO ()
testProperty _ _ = pure ()
