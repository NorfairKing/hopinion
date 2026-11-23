{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hopinion.ParseSpec (spec) where

import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.LanguageExtensions as LangExt
import Hopinion.Parse
import Path (relfile)
import Test.Syd

spec :: Spec
spec =
  describe "parseHaskellModule" $ do
    it "records an extension the cabal file turns on" $ do
      parsed <-
        parseHaskellModule
          ParseInput
            { parseInputRelPath = [relfile|src/Fixture.hs|],
              parseInputDefaultExtensions = ["StrictData"],
              parseInputSource = "module Fixture where\n"
            }
          >>= either (fail . show) pure
      EnumSet.member LangExt.StrictData (parsedModuleExtensions parsed) `shouldBe` True

    it "records an extension a pragma turns on, with the ones it implies" $ do
      parsed <-
        parseHaskellModule
          ParseInput
            { parseInputRelPath = [relfile|src/Fixture.hs|],
              parseInputDefaultExtensions = [],
              parseInputSource = "{-# LANGUAGE Strict #-}\nmodule Fixture where\n"
            }
          >>= either (fail . show) pure
      map (`EnumSet.member` parsedModuleExtensions parsed) [LangExt.Strict, LangExt.StrictData]
        `shouldBe` [True, True]

    it "records a pragma turning off an extension the cabal file turns on" $ do
      parsed <-
        parseHaskellModule
          ParseInput
            { parseInputRelPath = [relfile|src/Fixture.hs|],
              parseInputDefaultExtensions = ["StrictData"],
              parseInputSource = "{-# LANGUAGE NoStrictData #-}\nmodule Fixture where\n"
            }
          >>= either (fail . show) pure
      EnumSet.member LangExt.StrictData (parsedModuleExtensions parsed) `shouldBe` False

    it "records the extensions one the cabal file turns on implies" $ do
      parsed <-
        parseHaskellModule
          ParseInput
            { parseInputRelPath = [relfile|src/Fixture.hs|],
              parseInputDefaultExtensions = ["Strict", "TemplateHaskell"],
              parseInputSource = "module Fixture where\n"
            }
          >>= either (fail . show) pure
      map (`EnumSet.member` parsedModuleExtensions parsed) [LangExt.StrictData, LangExt.TemplateHaskellQuotes]
        `shouldBe` [True, True]

    it "records a pragma turning off an extension that one the cabal file turns on implies" $ do
      parsed <-
        parseHaskellModule
          ParseInput
            { parseInputRelPath = [relfile|src/Fixture.hs|],
              parseInputDefaultExtensions = ["Strict"],
              parseInputSource = "{-# LANGUAGE NoStrictData #-}\nmodule Fixture where\n"
            }
          >>= either (fail . show) pure
      map (`EnumSet.member` parsedModuleExtensions parsed) [LangExt.Strict, LangExt.StrictData]
        `shouldBe` [True, False]
