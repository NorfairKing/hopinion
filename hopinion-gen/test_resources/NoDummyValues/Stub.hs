-- | Stand-ins for sydtest, so that the compiler on the path can build the
-- fixture without the test suite's package database.
--
-- A module of their own, because what a spec defines itself counts as code
-- under test.
module Stub (Spec, describe, it, withCallback) where

type Spec = IO ()

describe :: String -> Spec -> Spec
describe _ s = s

it :: String -> Bool -> Spec
it _ _ = pure ()

withCallback :: ((String -> Bool) -> Bool) -> Bool
withCallback f = f null
