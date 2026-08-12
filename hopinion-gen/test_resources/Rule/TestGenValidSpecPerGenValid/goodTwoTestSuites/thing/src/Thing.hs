module Thing where

-- Tested by one test suite's Main.
data A = A

instance GenValid A

-- Tested by the other's, which is a different module that GHC also calls Main.
data B = B

instance GenValid B
