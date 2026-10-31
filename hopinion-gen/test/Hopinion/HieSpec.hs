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

import qualified Data.ByteString as SB
import Data.GenValidity.Text ()
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Hopinion.Hie
import Path (Abs, Dir, File, Path, Rel, reldir, relfile, toFilePath, (</>))
import Path.IO (copyFile, withSystemTempDir)
import System.Process (CreateProcess (..), proc, readCreateProcess)
import Test.Syd
import Test.Syd.Validity (forAllValid)

spec :: Spec
spec = do
  -- Both modules are resources rather than strings here, so that they read as
  -- the Haskell they are.
  let resourceDir :: Path Rel Dir
      resourceDir = [reldir|test_resources/Hie|]

  -- Compiled by the @ghc@ on the path, which is the one that built this suite,
  -- so the files under test are ones these readers are supposed to be able to
  -- read.
  --
  -- Compiled from inside the directory, and so named relatively, because a
  -- @.hie@ file records the path the compiler was handed and cabal hands
  -- relative ones. Compiling from outside would record an absolute path, which
  -- is a build whose modules 'compiledModuleFile' can tell apart from nothing.
  let withCompiled :: (Path Abs Dir -> IO ()) -> IO ()
      withCompiled act = withSystemTempDir "hopinion-hie" $ \tmp -> do
        copyFile (resourceDir </> [relfile|Generator.hs|]) (tmp </> [relfile|Generator.hs|])
        copyFile (resourceDir </> [relfile|Fixture.hs|]) (tmp </> [relfile|Fixture.hs|])
        _ <-
          readCreateProcess
            (proc "ghc" ["-v0", "-fwrite-ide-info", "-outputdir", ".", "-hiedir", ".", "-i.", "Fixture.hs"])
              { cwd = Just (toFilePath tmp)
              }
            ""
        act tmp

  let readingCompiled :: Path Abs File -> IO CompiledModule
      readingCompiled path = do
        result <- readCompiledModule path
        case result of
          Left err -> fail (T.unpack (renderArtifactUnreadable err))
          Right compiled -> pure compiled

  let readingInstances :: Path Abs File -> IO [DeclaredInstance]
      readingInstances path = do
        result <- readDeclaredInstances path
        case result of
          Left err -> fail (T.unpack (renderArtifactUnreadable err))
          Right instances -> pure instances

  describe "readCompiledModule" $ do
    -- The met side of an obligation rests on exactly this: two names that
    -- arrived together in one expansion. A module-wide name set would say only
    -- that both occur somewhere in the file.
    it "keeps the names of one splice expansion together" $
      withCompiled $ \tmp -> do
        compiled <- readingCompiled (tmp </> [relfile|Fixture.hie|])
        compiledModuleName compiled `shouldBe` "Fixture"
        compiledModuleGenerated compiled `shouldSatisfy` any (S.isSubsetOf (S.fromList ["probe", "Widget"]))

    it "does not claim two names arrived together when neither was generated" $
      withCompiled $ \tmp -> do
        compiled <- readingCompiled (tmp </> [relfile|Fixture.hie|])
        compiledModuleGenerated compiled `shouldSatisfy` not . any (S.isSubsetOf (S.fromList ["written", "NeverMentioned"]))

    it "records the file it was compiled from, which is what tells two Mains apart" $
      withCompiled $ \tmp -> do
        compiled <- readingCompiled (tmp </> [relfile|Fixture.hie|])
        -- As GHC was given it, which is why the path this is matched against is
        -- compared by trailing components rather than as text. See
        -- 'Hopinion.Compiled.sameSourceFile'.
        compiledModuleFile compiled `shouldBe` Just [relfile|Fixture.hs|]

    it "reports a file that is not a .hie file rather than throwing" $
      forAllValid $ \contents ->
        withSystemTempDir "hopinion-hie" $ \tmp -> do
          let path = tmp </> [relfile|Fixture.hie|]
          SB.writeFile (toFilePath path) (TE.encodeUtf8 contents)
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
        instances
          `shouldSatisfy` elem
            DeclaredInstance
              { declaredInstanceClass = "Gennable",
                declaredInstanceType = "Widget"
              }

    it "finds one the compiler derived, which is also in no source" $
      withCompiled $ \tmp -> do
        instances <- readingInstances (tmp </> [relfile|Fixture.hi|])
        instances
          `shouldSatisfy` elem
            DeclaredInstance
              { declaredInstanceClass = "Show",
                declaredInstanceType = "Written"
              }

    it "reports a file that is not an interface rather than throwing" $
      forAllValid $ \contents ->
        withSystemTempDir "hopinion-hie" $ \tmp -> do
          let path = tmp </> [relfile|Fixture.hi|]
          SB.writeFile (toFilePath path) (TE.encodeUtf8 contents)
          result <- readDeclaredInstances path
          case result of
            Left _ -> pure ()
            Right instances -> expectationFailure (unwords ["Read instances out of nothing:", show instances])
