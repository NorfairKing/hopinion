{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Which @.hie@ file belongs to which module, and whether a build accounted
-- for every module at all.
--
-- The interesting half of the first is the file, not the module name: every
-- component with a @main-is@ has one called @Main@, so the name alone would let
-- one test suite's compiled module be read as another's.
module Hopinion.CompiledSpec (spec) where

import qualified Data.ByteString as SB
import Data.GenValidity.ByteString ()
import Data.GenValidity.Path ()
import Hopinion.Compiled
import Hopinion.Facts.Gen ()
import Hopinion.Facts.Name
import Path (Dir, Path, Rel, parseRelFile, reldir, relfile, toFilePath, (</>))
import Path.IO (ensureDir, withSystemTempDir)
import Test.Syd
import Test.Syd.Validity (forAllValid)

spec :: Spec
spec = do
  -- What is in an artifact is read later, and only by the rules that ask, so
  -- whether a build covered a module cannot depend on it.
  describe "uncoveredModules" $ do
    -- No build was given, so nothing was promised. This is the module command
    -- and any run over a tree nobody compiled.
    it "promises nothing when there are no directories" $
      forAllValid $ \(key, file) -> do
        compiled <- compiledModulesFor noHieDirectories [(key, file)]
        uncovered <- uncoveredModules compiled
        uncovered `shouldBe` []

    -- The case the requirement exists for: a build was given and it does not
    -- account for a module that was read. At the point of use this is
    -- indistinguishable from the case above, since both say nothing, which is
    -- why it is caught here instead.
    -- [allow:TestNoDummyValues] One module the files are written for, and one they are not.
    it "names a module the directories do not account for" $
      forAllValid $ \contents ->
        withSystemTempDir "hopinion-hie" $ \dir -> do
          SB.writeFile (toFilePath (dir </> [relfile|Thing.hie|])) contents
          SB.writeFile (toFilePath (dir </> [relfile|Thing.hi|])) contents
          let thing = ModuleKey "Thing"
          let other = ModuleKey "Other"
          compiled <-
            compiledModulesFor
              (HieDirectories [dir])
              [ (thing, [relfile|thing/src/Thing.hs|]),
                (other, [relfile|thing/src/Other.hs|])
              ]
          uncovered <- uncoveredModules compiled
          uncovered `shouldBe` [[relfile|thing/src/Other.hs|]]

    -- Two files answer two questions, so a module with one of them is a module
    -- one question cannot be answered about, which is the thing this refuses to
    -- let pass quietly.
    it "names a module whose interface is missing beside its names" $
      forAllValid $ \contents ->
        withSystemTempDir "hopinion-hie" $ \dir -> do
          SB.writeFile (toFilePath (dir </> [relfile|Thing.hie|])) contents
          -- [allow:TestNoDummyValues] The module the file above was written for.
          let thing = ModuleKey "Thing"
          compiled <-
            compiledModulesFor
              (HieDirectories [dir])
              [(thing, [relfile|thing/src/Thing.hs|])]
          uncovered <- uncoveredModules compiled
          uncovered `shouldBe` [[relfile|thing/src/Thing.hs|]]

    -- A.B.C is A/B/C.hie, which is the layout the collection has to produce and
    -- the reason it is rooted where a module's own path starts.
    -- [allow:TestNoDummyValues] A top-level module and a dotted one, named for the files written for them.
    it "says nothing about a build that accounts for every module" $
      forAllValid $ \contents ->
        withSystemTempDir "hopinion-hie" $ \dir -> do
          SB.writeFile (toFilePath (dir </> [relfile|Thing.hie|])) contents
          SB.writeFile (toFilePath (dir </> [relfile|Thing.hi|])) contents
          ensureDir (dir </> [reldir|Deep/Down|])
          SB.writeFile (toFilePath (dir </> [relfile|Deep/Down/Other.hie|])) contents
          SB.writeFile (toFilePath (dir </> [relfile|Deep/Down/Other.hi|])) contents
          let thing = ModuleKey "Thing"
          let deepOther = ModuleKey "Deep.Down.Other"
          compiled <-
            compiledModulesFor
              (HieDirectories [dir])
              [ (thing, [relfile|thing/src/Thing.hs|]),
                (deepOther, [relfile|thing/src/Deep/Down/Other.hs|])
              ]
          uncovered <- uncoveredModules compiled
          uncovered `shouldBe` []

    -- Two components, two files, one module name. A directory each is what
    -- makes both answerable, and it is why a package's artifacts are one tree
    -- per component rather than one tree.
    it "counts both modules called Main covered when there is a tree for each" $
      forAllValid $ \contents ->
        withSystemTempDir "hopinion-hie" $ \dir -> do
          let one = dir </> [reldir|testa|]
          let two = dir </> [reldir|testb|]
          ensureDir one
          ensureDir two
          SB.writeFile (toFilePath (one </> [relfile|Main.hie|])) contents
          SB.writeFile (toFilePath (one </> [relfile|Main.hi|])) contents
          SB.writeFile (toFilePath (two </> [relfile|Main.hie|])) contents
          SB.writeFile (toFilePath (two </> [relfile|Main.hi|])) contents
          -- [allow:TestNoDummyValues] The name every component with a main-is compiles its main module as.
          let main' = ModuleKey "Main"
          compiled <-
            compiledModulesFor
              (HieDirectories [one, two])
              [ (main', [relfile|thing-gen/testa/Main.hs|]),
                (main', [relfile|thing-gen/testb/Main.hs|])
              ]
          uncovered <- uncoveredModules compiled
          uncovered `shouldBe` []

  describe "sameSourceFile" $ do
    it "matches a path relative to the package against one relative to the repository" $
      forAllValid $ \(package, directory, file) ->
        sameSourceFile (Just (directory </> file)) ((package :: Path Rel Dir) </> directory </> file)
          `shouldBe` True

    -- A build that hands the compiler absolute paths records absolute paths, and
    -- the directory it built in has nothing to do with the repository the module
    -- is named against. So it reads as a different file, which is the safe
    -- direction: the rule that asked learns nothing rather than the wrong thing.
    it "does not match a path from a build directory the repository knows nothing about" $
      sameSourceFile (parseRelFile "/build/thing-0.0.0/src/Thing.hs") [relfile|thing/src/Thing.hs|] `shouldBe` False

    -- The case the whole check exists for. Two test suites, two files, one module
    -- name, and one .hie file surviving the collection.
    it "tells one component's Main from another's" $ do
      let compiled = parseRelFile "testa/Main.hs"
      sameSourceFile compiled [relfile|thing-gen/testa/Main.hs|] `shouldBe` True
      sameSourceFile compiled [relfile|thing-gen/testb/Main.hs|] `shouldBe` False

    it "does not match a different module that happens to end the same way" $
      sameSourceFile (parseRelFile "src/Other/Thing.hs") [relfile|thing/src/Thing.hs|] `shouldBe` False

    it "ignores a leading dot, which is how a build often names its own directory" $
      sameSourceFile (parseRelFile "./src/Thing.hs") [relfile|thing/src/Thing.hs|] `shouldBe` True

    -- A file name on its own is not an identity, and this is the case that says
    -- so: Main.hs ends the path of every component that has a main-is, so
    -- accepting it would let one test suite's compiled module be read as
    -- another's, which is the whole reason the file is checked at all.
    it "refuses a bare file name, which would match every component's Main" $ do
      let compiled = parseRelFile "Main.hs"
      sameSourceFile compiled [relfile|thing-gen/testa/Main.hs|] `shouldBe` False
      sameSourceFile compiled [relfile|thing-gen/testb/Main.hs|] `shouldBe` False

    -- Agreeing about nothing read as agreement, because every pair drawn from an
    -- empty list agrees. Nothing is also what GHC recording an absolute path
    -- arrives as, one case up.
    it "refuses a path that is not one this tool could name" $
      forAllValid $ \file ->
        sameSourceFile Nothing file `shouldBe` False
