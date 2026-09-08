-- | Every directory of test resources belongs to exactly one spec, and says so
-- in its name: @test_resources/Comment@ belongs to @Hopinion.CommentSpec@.
--
-- That is what this asserts, in both directions. A directory nobody reads fails
-- here rather than sitting there being mistaken for coverage, and a directory
-- whose spec was deleted fails with it. Each owning spec then asserts the same
-- thing one level down, over its own contents.
module Hopinion.TestResourcesSpec (spec) where

import Hopinion.TestUtils (ownedSpec, resourcesDir, specDir)
import Path.IO (listDirRel)
import Test.Syd

spec :: Spec
spec = do
  (entries, _) <- runIO (listDirRel resourcesDir)
  -- The spec directory listed once rather than a question asked per resource:
  -- one answer about what is there cannot be true for one entry and false for
  -- the next.
  (_, specs) <- runIO (listDirRel specDir)
  mapM_ (ownedSpec specs) entries
