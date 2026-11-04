module Thing.TestMain (testMain) where

import Thing.Spec (thingSpec)

testMain :: IO ()
testMain = thingSpec
