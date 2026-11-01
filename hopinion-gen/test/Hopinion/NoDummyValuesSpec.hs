{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The rule with a build behind it, which the resource cases do not have.
--
-- Compiled here by the @ghc@ on the path, for the reason given in
-- "Hopinion.HieSpec".
module Hopinion.NoDummyValuesSpec (spec) where

import qualified Data.Text.IO as TIO
import Hopinion.Compiled
import Hopinion.Facts.Component
import Hopinion.Facts.Name
import Hopinion.Report
import Hopinion.Rule
import Hopinion.Rule.Gen (shippedRules)
import Hopinion.Rule.Id
import Hopinion.Run
import Path (reldir, relfile, toFilePath, (</>))
import Path.IO (copyFile, ensureDir, withSystemTempDir)
import System.Process (CreateProcess (..), proc, readCreateProcess)
import Test.Syd

spec :: Spec
spec = do
  -- In a directory, because a build is matched to a module by at least two
  -- components of the path the compiler was handed.
  let source = [relfile|test/ShadowSpec.hs|]

  describe "the names in a module" $ do
    -- [allow:TestNoDummyValues] The module name is the fixture's, which the
    -- build is looked up by.
    it "are resolved by the build" $
      withSystemTempDir "hopinion-no-dummy-values" $ \tmp -> do
        ensureDir (tmp </> [reldir|test|])
        copyFile [relfile|test_resources/NoDummyValues/ShadowSpec.hs|] (tmp </> source)
        copyFile [relfile|test_resources/NoDummyValues/Stub.hs|] (tmp </> [relfile|test/Stub.hs|])
        _ <-
          readCreateProcess
            (proc "ghc" ["-v0", "-fwrite-ide-info", "-outputdir", ".", "-hiedir", ".", "-itest", toFilePath source])
              { cwd = Just (toFilePath tmp)
              }
            ""
        contents <- TIO.readFile (toFilePath (tmp </> source))
        ctx <- factsForSource shippedRules source [] ComponentKindTest contents
        compiled <- compiledModulesFor (HieDirectories [tmp]) [(ModuleKey "ShadowSpec", source)]
        report <-
          runModuleLayer
            shippedRules
            ModuleBuild
              { moduleBuildProjectPackages = [],
                moduleBuildCompiled = compiled
              }
            ctx
        [ findingMessage f
          | f <- complaintsFindings report,
            findingRule f == RuleId "TestNoDummyValues"
          ]
          `shouldBe` [ "Literals a test runs on: \"erin\".",
                       "Literals a test runs on: \"gina\".",
                       "Literals a test runs on: \"ruth\"."
                     ]

    it "are matched by spelling without one" $ do
      contents <- TIO.readFile "test_resources/NoDummyValues/ShadowSpec.hs"
      ctx <- factsForSource shippedRules source [] ComponentKindTest contents
      report <- runModuleLayer shippedRules noModuleBuild ctx
      [ findingMessage f
        | f <- complaintsFindings report,
          findingRule f == RuleId "TestNoDummyValues"
        ]
        `shouldBe` [ "Literals a test runs on: \"erin\".",
                     "Literals a test runs on: \"frank\".",
                     "Literals a test runs on: \"gina\".",
                     "Literals a test runs on: \"ivan\".",
                     "Literals a test runs on: \"ruth\"."
                   ]
