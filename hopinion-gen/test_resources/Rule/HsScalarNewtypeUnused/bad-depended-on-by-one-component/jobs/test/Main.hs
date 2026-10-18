module Main where

main :: IO ()
main = pure ()

-- The same field in the component that does build against it, which is the one
-- finding this case has. A dependency belongs to the component that declares
-- it, so which component a site is in is what decides whether the type is
-- within reach.
data Expectation = Expectation
  { expectationInstallationId :: !Int
  }
