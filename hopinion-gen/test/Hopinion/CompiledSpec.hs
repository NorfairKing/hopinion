{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Which @.hie@ file belongs to which module, and whether a build accounted
-- for every module at all.
--
-- The interesting half of the first is the file, not the module name: every
-- component with a @main-is@ has one called @Main@, so the name alone would let
-- one test suite's compiled module be read as another's.
module Hopinion.CompiledSpec (spec) where

import Hopinion.Compiled
import Hopinion.Facts.Name
import Hopinion.Hie (DeclaredInstance (..), compiledModuleName)
import Path (parseRelFile, reldir, relfile, toFilePath, (</>))
import Path.IO (copyFile, ensureDir, removeFile, withSystemTempDir)
import System.Process (CreateProcess (..), proc, readCreateProcess)
import Test.Syd

spec :: Spec
spec = do
  describe "uncoveredModules" $ do
    -- No build was given, so nothing was promised. This is the module command
    -- and any run over a tree nobody compiled.
    it "promises nothing when there are no directories" $ do
      compiled <-
        compiledModulesFor
          noHieDirectories
          [(ModuleKey "Thing", [relfile|thing/src/Thing.hs|])]
      uncovered <- uncoveredModules compiled
      uncovered `shouldBe` []

    -- The case the requirement exists for: a build was given and it does not
    -- account for a module that was read. At the point of use this is
    -- indistinguishable from the case above, since both say nothing, which is
    -- why it is caught here instead.
    it "names a module the directories do not account for" $
      withSystemTempDir "hopinion-hie" $ \dir -> do
        writeFile (toFilePath (dir </> [relfile|Thing.hie|])) ""
        writeFile (toFilePath (dir </> [relfile|Thing.hi|])) ""
        compiled <-
          compiledModulesFor
            (HieDirectories [dir])
            [ (ModuleKey "Thing", [relfile|thing/src/Thing.hs|]),
              (ModuleKey "Other", [relfile|thing/src/Other.hs|])
            ]
        uncovered <- uncoveredModules compiled
        uncovered `shouldBe` [[relfile|thing/src/Other.hs|]]

    -- Two files answer two questions, so a module with one of them is a module
    -- one question cannot be answered about, which is the thing this refuses to
    -- let pass quietly.
    it "names a module whose interface is missing beside its names" $
      withSystemTempDir "hopinion-hie" $ \dir -> do
        writeFile (toFilePath (dir </> [relfile|Thing.hie|])) ""
        compiled <-
          compiledModulesFor
            (HieDirectories [dir])
            [(ModuleKey "Thing", [relfile|thing/src/Thing.hs|])]
        uncovered <- uncoveredModules compiled
        uncovered `shouldBe` [[relfile|thing/src/Thing.hs|]]

    -- A.B.C is A/B/C.hie, which is the layout the collection has to produce and
    -- the reason it is rooted where a module's own path starts.
    it "says nothing about a build that accounts for every module" $
      withSystemTempDir "hopinion-hie" $ \dir -> do
        writeFile (toFilePath (dir </> [relfile|Thing.hie|])) ""
        writeFile (toFilePath (dir </> [relfile|Thing.hi|])) ""
        ensureDir (dir </> [reldir|Deep/Down|])
        writeFile (toFilePath (dir </> [relfile|Deep/Down/Other.hie|])) ""
        writeFile (toFilePath (dir </> [relfile|Deep/Down/Other.hi|])) ""
        compiled <-
          compiledModulesFor
            (HieDirectories [dir])
            [ (ModuleKey "Thing", [relfile|thing/src/Thing.hs|]),
              (ModuleKey "Deep.Down.Other", [relfile|thing/src/Deep/Down/Other.hs|])
            ]
        uncovered <- uncoveredModules compiled
        uncovered `shouldBe` []

    -- Two components, two files, one module name. A directory each is what
    -- makes both answerable, and it is why a package's artifacts are one tree
    -- per component rather than one tree.
    it "counts both modules called Main covered when there is a tree for each" $
      withSystemTempDir "hopinion-hie" $ \dir -> do
        let one = dir </> [reldir|testa|]
        let two = dir </> [reldir|testb|]
        ensureDir one
        ensureDir two
        writeFile (toFilePath (one </> [relfile|Main.hie|])) ""
        writeFile (toFilePath (one </> [relfile|Main.hi|])) ""
        writeFile (toFilePath (two </> [relfile|Main.hie|])) ""
        writeFile (toFilePath (two </> [relfile|Main.hi|])) ""
        compiled <-
          compiledModulesFor
            (HieDirectories [one, two])
            [ (ModuleKey "Main", [relfile|thing-gen/testa/Main.hs|]),
              (ModuleKey "Main", [relfile|thing-gen/testb/Main.hs|])
            ]
        uncovered <- uncoveredModules compiled
        uncovered `shouldBe` []

  -- Reading a module's artifacts costs more than parsing it did, so each file
  -- is read once however many rules ask, and in whichever order they ask.
  -- A file that is gone after the first question is how a second read shows:
  -- the second question would find nothing there.
  describe "reading each file once" $ do
    let fixture :: String
        fixture =
          unlines
            [ "module Fixture where",
              "",
              "data Written = Written",
              "  deriving (Show)"
            ]
    let rp = [relfile|thing/src/Fixture.hs|]
    let askNames :: CompiledModules -> IO ()
        askNames compiled = do
          named <- compiledModuleOf rp compiled
          fmap compiledModuleName named `shouldBe` Just "Fixture"
    let askInterface :: CompiledModules -> IO ()
        askInterface compiled = do
          declared <- declaredInstancesOf rp compiled
          declared
            `shouldBe` Just
              [ DeclaredInstance
                  { declaredInstanceClass = "Show",
                    declaredInstanceType = "Written"
                  }
              ]
    let askingTwice :: (CompiledModules -> IO ()) -> (CompiledModules -> IO ()) -> IO ()
        askingTwice first second = withSystemTempDir "hopinion-hie" $ \tmp -> do
          ensureDir (tmp </> [reldir|src|])
          writeFile (toFilePath (tmp </> [relfile|src/Fixture.hs|])) fixture
          _ <-
            readCreateProcess
              (proc "ghc" ["-v0", "-fwrite-ide-info", "-outputdir", ".", "-hiedir", ".", "src/Fixture.hs"])
                { cwd = Just (toFilePath tmp)
                }
              ""
          compiled <-
            compiledModulesFor
              (HieDirectories [tmp])
              [(ModuleKey "Fixture", rp)]
          first compiled
          removeFile (tmp </> [relfile|Fixture.hie|])
          second compiled

    it "reads the .hie file once when its names are asked for first" $
      askingTwice askNames askInterface

    it "reads the .hie file once when the interface beside it is asked for first" $
      askingTwice askInterface askNames

    -- Covered means answerable: a module the coverage check lets through is
    -- one whose interface a rule can then be given.
    it "answers for an interface wherever coverage found one" $
      withSystemTempDir "hopinion-hie" $ \tmp -> do
        let partial = tmp </> [reldir|partial|]
        let whole = tmp </> [reldir|whole|]
        ensureDir partial
        ensureDir (whole </> [reldir|src|])
        writeFile (toFilePath (whole </> [relfile|src/Fixture.hs|])) fixture
        _ <-
          readCreateProcess
            (proc "ghc" ["-v0", "-fwrite-ide-info", "-outputdir", ".", "-hiedir", ".", "src/Fixture.hs"])
              { cwd = Just (toFilePath whole)
              }
            ""
        copyFile (whole </> [relfile|Fixture.hie|]) (partial </> [relfile|Fixture.hie|])
        compiled <-
          compiledModulesFor
            (HieDirectories [partial, whole])
            [(ModuleKey "Fixture", rp)]
        uncovered <- uncoveredModules compiled
        uncovered `shouldBe` []
        askInterface compiled

  describe "sameSourceFile" $ do
    it "matches a path relative to the package against one relative to the repository" $
      sameSourceFile (parseRelFile "src/Thing.hs") [relfile|thing/src/Thing.hs|] `shouldBe` True

    -- A build that hands the compiler absolute paths records absolute paths, and
    -- the directory it built in has nothing to do with the repository the module
    -- is named against. So it reads as a different file, which is the safe
    -- direction: the rule that asked learns nothing rather than the wrong thing.
    it "does not match a path from a build directory the repository knows nothing about" $
      sameSourceFile (parseRelFile "/build/thing-0.0.0/src/Thing.hs") [relfile|thing/src/Thing.hs|] `shouldBe` False

    -- The case the whole check exists for. Two test suites, two files, one module
    -- name, and one .hie file surviving the collection.
    it "tells one component's Main from another's" $ do
      sameSourceFile (parseRelFile "testa/Main.hs") [relfile|thing-gen/testa/Main.hs|] `shouldBe` True
      sameSourceFile (parseRelFile "testa/Main.hs") [relfile|thing-gen/testb/Main.hs|] `shouldBe` False

    it "does not match a different module that happens to end the same way" $
      sameSourceFile (parseRelFile "src/Other/Thing.hs") [relfile|thing/src/Thing.hs|] `shouldBe` False

    it "ignores a leading dot, which is how a build often names its own directory" $
      sameSourceFile (parseRelFile "./src/Thing.hs") [relfile|thing/src/Thing.hs|] `shouldBe` True

    -- A file name on its own is not an identity, and this is the case that says
    -- so: Main.hs ends the path of every component that has a main-is, so
    -- accepting it would let one test suite's compiled module be read as
    -- another's, which is the whole reason the file is checked at all.
    it "refuses a bare file name, which would match every component's Main" $ do
      sameSourceFile (parseRelFile "Main.hs") [relfile|thing-gen/testa/Main.hs|] `shouldBe` False
      sameSourceFile (parseRelFile "Main.hs") [relfile|thing-gen/testb/Main.hs|] `shouldBe` False

    -- Agreeing about nothing read as agreement, because every pair drawn from an
    -- empty list agrees. Nothing is also what GHC recording an absolute path
    -- arrives as, one case up.
    it "refuses a path that is not one this tool could name" $
      sameSourceFile Nothing [relfile|thing/src/Thing.hs|] `shouldBe` False
