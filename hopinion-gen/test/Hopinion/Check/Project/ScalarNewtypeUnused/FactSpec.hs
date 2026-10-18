{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hopinion.Check.Project.ScalarNewtypeUnused.FactSpec (spec) where

import qualified Data.Set as S
import Database.Persist (PersistField (..))
import Hopinion.Check.Project.ScalarNewtypeUnused.Fact
import Hopinion.Facts.Export (ExportEntry (..), ExportList (..))
import Hopinion.Facts.Name
import Hopinion.Facts.Place
import Hopinion.Facts.Primitive
import Path (relfile)
import Test.Syd

spec :: Spec
spec = do
  -- Where a span points is nothing these answers depend on, and every
  -- ExportList carries one.
  let sp =
        Span
          { spanFile = [relfile|Thing.hs|],
            spanStart =
              Position
                { positionLine = 1,
                  positionCol = 1
                },
            spanEnd =
              Position
                { positionLine = 1,
                  positionCol = 1
                }
          }
  describe "wordsOfName" $ do
    it "reads a single word as one word" $
      wordsOfName "Username" `shouldBe` ["Username"]

    it "splits where a capital begins a word" $
      wordsOfName "gitUserName" `shouldBe` ["git", "User", "Name"]

    -- A run of capitals is one word rather than a word per letter, and the run
    -- gives its last letter to the word that follows it.
    it "reads a run of capitals as one word" $
      wordsOfName "fooJSONUrl" `shouldBe` ["foo", "JSON", "Url"]

    it "keeps a trailing run of capitals whole" $
      wordsOfName "parseJSON" `shouldBe` ["parse", "JSON"]

    it "keeps digits with the word they are written in" $
      wordsOfName "e2eOIDCSettingClientSecret"
        `shouldBe` ["e2e", "OIDC", "Setting", "Client", "Secret"]

    it "reads a name with no capitals as one word" $
      wordsOfName "myhostname" `shouldBe` ["myhostname"]

  describe "endsInTypeName" $ do
    it "matches a field that is the type's name and nothing else" $
      endsInTypeName "Hostname" "hostname" `shouldBe` True

    -- The letters of Username are the end of gitUserName, and a git author's
    -- name is not a platform username. What tells them apart is that one is two
    -- words and the other is one, which is why both names are split rather than
    -- only the field's.
    it "does not match letters that fall across a word boundary" $
      endsInTypeName "Username" "gitUserName" `shouldBe` False

    it "matches the same letters when the words do line up" $
      endsInTypeName "Username" "pushEventUserUsername" `shouldBe` True

    it "matches a field that is the type's name after a record prefix" $
      endsInTypeName "Hostname" "settingsHostname" `shouldBe` True

    it "matches across a run of capitals the two names spell differently" $
      endsInTypeName "JsonUrl" "fooJSONUrl" `shouldBe` True

    -- The boundary is what separates a field that is a hostname from a field
    -- whose name merely ends in those letters, and it is the only thing doing
    -- that work: both of these have the type's name on the end.
    it "does not match letters that are the tail of a longer word" $
      endsInTypeName "Hostname" "myhostname" `shouldBe` False

    it "does not match a word the field's own word ends in" $
      endsInTypeName "Name" "userFirstname" `shouldBe` False

    -- The case the rule was never measured against, because none of the
    -- repositories it was measured on names a scalar in one syllable. A short
    -- name is what would misfire worst without the boundary, so it is pinned
    -- here rather than left to be discovered.
    it "does not match a short name that lands inside a word" $
      endsInTypeName "Id" "userValid" `shouldBe` False

    it "matches a short name on a boundary" $
      endsInTypeName "Id" "userId" `shouldBe` True

    it "does not match a field shorter than the type's name" $
      endsInTypeName "Hostname" "host" `shouldBe` False

  describe "reachablePackages" $ do
    it "reaches the package the site is in" $
      reachablePackages
        [PackageName "thing"]
        []
        (PackageName "thing")
        mainLibraryComponent
        `shouldBe` S.fromList [PackageName "thing"]

    it "reaches a package the site's own component builds against" $
      reachablePackages
        [PackageName "thing", PackageName "jobs"]
        [(PackageName "jobs", mainLibraryComponent, PackageName "thing")]
        (PackageName "jobs")
        mainLibraryComponent
        `shouldBe` S.fromList [PackageName "jobs", PackageName "thing"]

    it "reaches a package a dependency's main library builds against" $
      reachablePackages
        [PackageName "thing", PackageName "middle", PackageName "jobs"]
        [ (PackageName "jobs", mainLibraryComponent, PackageName "middle"),
          (PackageName "middle", mainLibraryComponent, PackageName "thing")
        ]
        (PackageName "jobs")
        mainLibraryComponent
        `shouldBe` S.fromList [PackageName "jobs", PackageName "middle", PackageName "thing"]

    -- A dependency a test suite declares is one the library cannot use, so a
    -- hop through a package is a hop through its main library and nothing else.
    it "does not hop through a dependency another component of the package declares" $
      reachablePackages
        [PackageName "thing", PackageName "middle", PackageName "jobs"]
        [ (PackageName "jobs", mainLibraryComponent, PackageName "middle"),
          (PackageName "middle", ComponentName "test", PackageName "thing")
        ]
        (PackageName "jobs")
        mainLibraryComponent
        `shouldBe` S.fromList [PackageName "jobs", PackageName "middle"]

    it "reaches what the site's own test suite builds against when that is the component asking" $
      reachablePackages
        [PackageName "thing", PackageName "jobs"]
        [(PackageName "jobs", ComponentName "test", PackageName "thing")]
        (PackageName "jobs")
        (ComponentName "test")
        `shouldBe` S.fromList [PackageName "jobs", PackageName "thing"]

    -- A cabal file names mostly Hackage, and what those packages depend on is
    -- not read here, so a name outside the project is dropped rather than
    -- followed.
    it "drops a dependency that is not a package in the project" $
      reachablePackages
        [PackageName "jobs"]
        [(PackageName "jobs", mainLibraryComponent, PackageName "text")]
        (PackageName "jobs")
        mainLibraryComponent
        `shouldBe` S.fromList [PackageName "jobs"]

    it "terminates on a cycle" $
      reachablePackages
        [PackageName "a", PackageName "b"]
        [ (PackageName "a", mainLibraryComponent, PackageName "b"),
          (PackageName "b", mainLibraryComponent, PackageName "a")
        ]
        (PackageName "a")
        mainLibraryComponent
        `shouldBe` S.fromList [PackageName "a", PackageName "b"]

  describe "constructorIsExported" $ do
    -- A module with no export list exports what it declares, so there is
    -- nothing kept in to find.
    it "exposes everything when there is no export list" $
      constructorIsExported ExportListNone (TypeHead "Width") "Width" `shouldBe` True

    -- A newtype and its constructor are usually written under one name, so a
    -- bare entry looks exactly like the constructor and is not it: a
    -- constructor leaves a module only inside its type's subordinate list.
    it "does not expose a constructor behind a bare type name" $
      constructorIsExported (ExportList sp [ExportEntry "Width"]) (TypeHead "Width") "Width"
        `shouldBe` False

    it "exposes a constructor under two dots" $
      constructorIsExported (ExportList sp [ExportEntry "Width(..)"]) (TypeHead "Width") "Width"
        `shouldBe` True

    it "exposes a constructor the list names" $
      constructorIsExported
        (ExportList sp [ExportEntry "Width(Width, unWidth)"])
        (TypeHead "Width")
        "Width"
        `shouldBe` True

    it "does not expose a constructor a list leaves out" $
      constructorIsExported (ExportList sp [ExportEntry "Width(unWidth)"]) (TypeHead "Width") "Width"
        `shouldBe` False

    -- Asking only whether some entry opens its subordinates had one Thing (..)
    -- anywhere in a module answer for every constructor the module declares,
    -- which is how a Width nobody can build was named as a fix.
    it "does not let another type's two dots expose this constructor" $
      constructorIsExported
        (ExportList sp [ExportEntry "Dashboard(..)", ExportEntry "Width"])
        (TypeHead "Width")
        "Width"
        `shouldBe` False

    it "says nothing is exposed by a list that does not mention the type" $
      constructorIsExported (ExportList sp [ExportEntry "halfWidth"]) (TypeHead "Width") "Width"
        `shouldBe` False

  describe "primitiveNamed" $ do
    it "refuses a type this rule does not read as a primitive" $
      primitiveNamed "Hostname" `shouldBe` Nothing

    it "reads back every primitive it stores" $
      let roundtrip t = do
            p <- primitiveNamed t
            case fromPersistValue (toPersistValue p) of
              Left _ -> Nothing
              Right p' -> Just (p' :: Primitive)
       in map roundtrip ["Text", "String", "ByteString", "Int", "Word8", "Rational"]
            `shouldBe` map primitiveNamed ["Text", "String", "ByteString", "Int", "Word8", "Rational"]
