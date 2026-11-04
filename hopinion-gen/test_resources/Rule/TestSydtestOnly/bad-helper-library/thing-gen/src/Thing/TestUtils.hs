module Thing.TestUtils (assertDoubles) where

import Test.HUnit ((@?=))

assertDoubles :: Int -> IO ()
assertDoubles n = (n + n) @?= (2 * n)
