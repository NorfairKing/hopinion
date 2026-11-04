-- | Enough of hspec's interface for a suite written against it to compile.
module Test.Hspec.QuickCheck (prop) where

prop :: String -> (Int -> Bool) -> IO ()
prop _ _ = pure ()
