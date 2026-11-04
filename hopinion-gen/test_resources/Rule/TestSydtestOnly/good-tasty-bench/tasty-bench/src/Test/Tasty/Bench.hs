-- | Enough of tasty-bench's interface for a benchmark written against it to
-- compile.
module Test.Tasty.Bench (Benchmark, bench, defaultMain, whnf) where

type Benchmark = IO ()

bench :: String -> IO () -> Benchmark
bench _ act = act

defaultMain :: [Benchmark] -> IO ()
defaultMain = sequence_

whnf :: (a -> b) -> a -> IO ()
whnf f x = f x `seq` pure ()
