{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Every directory of test resources belongs to exactly one spec, and says so
-- in its name: @test_resources/Comment@ belongs to @Hopinion.CommentSpec@.
--
-- That is what this asserts, in both directions. A directory nobody reads fails
-- here rather than sitting there being mistaken for coverage, and a directory
-- whose spec was deleted fails with it. Each owning spec then asserts the same
-- thing one level down, over its own contents.
module Hopinion.TestResourcesSpec (spec) where

import Path (Dir, File, Path, Rel, dirname, parseRelFile, reldir, toFilePath, (</>))
import Path.IO (listDirRel)
import System.FilePath (dropTrailingPathSeparator)
import Test.Syd

resourceDir :: Path Rel Dir
resourceDir = [reldir|test_resources|]

specDir :: Path Rel Dir
specDir = [reldir|test/Hopinion|]

spec :: Spec
spec = do
  (entries, _) <- runIO (listDirRel resourceDir)
  -- The spec directory listed once rather than a question asked per resource:
  -- one answer about what is there cannot be true for one entry and false for
  -- the next.
  (_, specs) <- runIO (listDirRel specDir)
  mapM_ (ownedSpec specs) entries

ownedSpec :: [Path Rel File] -> Path Rel Dir -> Spec
ownedSpec specs entry =
  it (unwords [name, concat ["belongs to Hopinion.", name, "Spec"]]) $ do
    expected <- parseRelFile (concat [name, "Spec.hs"])
    if expected `elem` specs
      then pure ()
      else
        expectationFailure
          ( unwords
              [ "The resources in",
                toFilePath (resourceDir </> entry),
                "have no spec.",
                "Either read them from",
                toFilePath (specDir </> expected),
                "or delete them."
              ]
          )
  where
    -- A directory's name and the first half of its spec's name are the same
    -- word, and the separator a directory's rendering ends in is the only thing
    -- between them.
    name :: String
    name = dropTrailingPathSeparator (toFilePath (dirname entry))
