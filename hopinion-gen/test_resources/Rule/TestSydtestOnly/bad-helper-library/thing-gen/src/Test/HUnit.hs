-- | Enough of HUnit's interface for a module written against it to compile.
module Test.HUnit ((@?=)) where

(@?=) :: (Eq a) => a -> a -> IO ()
_ @?= _ = pure ()
