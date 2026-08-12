{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The two readers, against files a compiler really wrote.
--
-- This is the one component that is locked to a compiler version, and it is
-- exercised end to end only through the Nix checks, three derivations away from
-- where a failure would be. So the fixture is compiled here, by the @ghc@ that
-- is on the path, which is the same one that built this suite.
--
-- The fixture really splices, because everything these readers exist for is
-- about code that is not in the source. Two modules, because Template Haskell
-- will not run a splice defined in the module that splices it, which is also
-- why the assertions below can be about names that appear in one file and not
-- the other.
module Hopinion.HieSpec (spec) where

import qualified Data.Set as S
import qualified Data.Text as T
import Hopinion.Hie
import Path (Abs, Dir, File, Path, relfile, toFilePath, (</>))
import Path.IO (withSystemTempDir)
import System.Process (CreateProcess (..), proc, readCreateProcess)
import Test.Syd

spec :: Spec
spec = do
  describe "readCompiledModule" $ do
    -- The met side of an obligation rests on exactly this: two names that
    -- arrived together in one expansion. A module-wide name set would say only
    -- that both occur somewhere in the file.
    it "keeps the names of one splice expansion together" $
      withCompiled $ \tmp -> do
        compiled <- readingCompiled (tmp </> [relfile|Fixture.hie|])
        compiledModuleName compiled `shouldBe` "Fixture"
        let together names = S.member "probe" names && S.member "Widget" names
        any together (compiledModuleGenerated compiled) `shouldBe` True

    it "does not claim two names arrived together when neither was generated" $
      withCompiled $ \tmp -> do
        compiled <- readingCompiled (tmp </> [relfile|Fixture.hie|])
        let together names = S.member "written" names && S.member "NeverMentioned" names
        any together (compiledModuleGenerated compiled) `shouldBe` False

    it "records the file it was compiled from, which is what tells two Mains apart" $
      withCompiled $ \tmp -> do
        compiled <- readingCompiled (tmp </> [relfile|Fixture.hie|])
        -- As GHC was given it, which is why the path this is matched against is
        -- compared by trailing components rather than as text. See
        -- 'Hopinion.Compiled.sameSourceFile'.
        compiledModuleFile compiled `shouldBe` Just [relfile|Fixture.hs|]

    it "reports a file that is not a .hie file rather than throwing" $
      withSystemTempDir "hopinion-hie" $ \tmp -> do
        let path = tmp </> [relfile|Fixture.hie|]
        writeFile (toFilePath path) "this is not a hie file"
        result <- readCompiledModule path
        case result of
          Left _ -> pure ()
          Right compiled -> expectationFailure (unwords ["Read a module out of nothing:", show compiled])

  describe "readDeclaredInstances" $ do
    -- The made side of an obligation rests on this: an instance that is in the
    -- module and in no source anybody wrote. Nothing in Fixture.hs mentions
    -- Gennable at all.
    it "finds an instance a splice generated" $
      withCompiled $ \tmp -> do
        instances <- readingInstances (tmp </> [relfile|Fixture.hi|])
        DeclaredInstance {declaredInstanceClass = "Gennable", declaredInstanceType = "Widget"}
          `elem` instances
          `shouldBe` True

    it "finds one the compiler derived, which is also in no source" $
      withCompiled $ \tmp -> do
        instances <- readingInstances (tmp </> [relfile|Fixture.hi|])
        DeclaredInstance {declaredInstanceClass = "Show", declaredInstanceType = "Written"}
          `elem` instances
          `shouldBe` True

    it "reports a file that is not an interface rather than throwing" $
      withSystemTempDir "hopinion-hie" $ \tmp -> do
        let path = tmp </> [relfile|Fixture.hi|]
        writeFile (toFilePath path) "this is not an interface"
        result <- readDeclaredInstances path
        case result of
          Left _ -> pure ()
          Right instances -> expectationFailure (unwords ["Read instances out of nothing:", show instances])

-- | What the splice is made of. Defined here rather than in the module that
-- splices it, because a quotation is resolved where it is written and a splice
-- cannot run code from its own module.
generator :: String
generator =
  unlines
    [ "{-# LANGUAGE TemplateHaskell #-}",
      "module Generator (Widget (..), Gennable, probe, spliceInstance, spliceCall) where",
      "",
      "import Language.Haskell.TH (Dec, Exp, Q)",
      "",
      "data Widget = Widget",
      "",
      "class Gennable a",
      "",
      "probe :: Widget -> ()",
      "probe _ = ()",
      "",
      "spliceInstance :: Q [Dec]",
      "spliceInstance = [d| instance Gennable Widget |]",
      "",
      "spliceCall :: Q Exp",
      "spliceCall = [| probe Widget |]"
    ]

-- | Nothing here names @Gennable@, and nothing here names @probe@ at a
-- @Widget@. Both are in the compiled module all the same, which is the whole
-- reason these files are read.
fixture :: String
fixture =
  unlines
    [ "{-# LANGUAGE TemplateHaskell #-}",
      "module Fixture where",
      "",
      "import Generator",
      "",
      "data Written = Written",
      "  deriving (Show)",
      "",
      "written :: Written",
      "written = Written",
      "",
      "$(spliceInstance)",
      "",
      "called :: ()",
      "called = $(spliceCall)"
    ]

-- | Compiled by the @ghc@ on the path, which is the one that built this suite,
-- so the files under test are ones these readers are supposed to be able to
-- read.
--
-- Compiled from inside the directory, and so named relatively, because a @.hie@
-- file records the path the compiler was handed and cabal hands relative ones.
-- Compiling from outside would record an absolute path, which is a build whose
-- modules 'compiledModuleFile' can tell apart from nothing.
withCompiled :: (Path Abs Dir -> IO ()) -> IO ()
withCompiled act = withSystemTempDir "hopinion-hie" $ \tmp -> do
  writeFile (toFilePath (tmp </> [relfile|Generator.hs|])) generator
  writeFile (toFilePath (tmp </> [relfile|Fixture.hs|])) fixture
  _ <-
    readCreateProcess
      (proc "ghc" ["-v0", "-fwrite-ide-info", "-outputdir", ".", "-hiedir", ".", "-i.", "Fixture.hs"])
        { cwd = Just (toFilePath tmp)
        }
      ""
  act tmp

readingCompiled :: Path Abs File -> IO CompiledModule
readingCompiled path = do
  result <- readCompiledModule path
  case result of
    Left err -> fail (T.unpack (renderArtifactUnreadable err))
    Right compiled -> pure compiled

readingInstances :: Path Abs File -> IO [DeclaredInstance]
readingInstances path = do
  result <- readDeclaredInstances path
  case result of
    Left err -> fail (T.unpack (renderArtifactUnreadable err))
    Right instances -> pure instances
