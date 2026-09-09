{-# LANGUAGE QuasiQuotes #-}

module Hopinion.Facts.ModuleSpec (spec) where

import Hopinion.Facts.Module (isSpecFilePath)
import Path (relfile)
import Test.Syd

spec :: Spec
spec =
  describe "isSpecFilePath" $ do
    it "reads the file name rather than the path it is under" $ do
      isSpecFilePath [relfile|test/Hopinion/CommentSpec.hs|] `shouldBe` True
      isSpecFilePath [relfile|CommentSpec.hs|] `shouldBe` True

    -- Spec.hs is what sydtest-discover generates the entry point from, and a
    -- preprocessor writes its module header, so the rules that read a test
    -- file's export list or its bindings would be reading a file that has
    -- neither and can be given neither.
    it "does not read the discovery entry point as a test file" $ do
      isSpecFilePath [relfile|test/Spec.hs|] `shouldBe` False
      isSpecFilePath [relfile|Spec.hs|] `shouldBe` False

    it "does not read a module that merely mentions a spec as a test file" $ do
      isSpecFilePath [relfile|src/Hopinion/Rule.hs|] `shouldBe` False
      isSpecFilePath [relfile|test/SpecHelpers.hs|] `shouldBe` False
