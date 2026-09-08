-- | Documentation in the export list is not an export: a section heading
-- names nothing, so this file still exports exactly spec.
module GoodADocumentedExportSpec
  ( -- * The spec
    spec,
  )
where

import Test.Syd

spec :: Spec
spec = it "adds" (1 + 1 `shouldBe` (2 :: Int))
