{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Hopinion.CommentSpec (spec) where

import Control.Monad (unless)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hopinion.Comment
import Hopinion.Facts
import Hopinion.Facts.Gen ()
import Hopinion.Rule.Gen (shippedRules)
import Hopinion.Run (factsForFile, factsForSource)
import Path (Dir, File, Path, Rel, addExtension, fileExtension, filename, parent, parseRelFile, reldir, relfile, toFilePath, (</>))
import Path.IO (forgivingAbsence, getModificationTime)
import System.Process (readProcess)
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity

-- | One module per attachment case, each beside the golden of what hopinion
-- makes of it.
resourceDir :: Path Rel Dir
resourceDir = [reldir|test_resources/Comment|]

spec :: Spec
spec = do
  describe "RawComment" $ genValidSpec @RawComment

  describe "commentBlocks" $
    it "puts every comment in exactly one block, in source order" $
      forAllValid $ \ls ->
        forAllValid $ \comments -> do
          let ctx = mkCommentContext ls comments [] Nothing Nothing
          let blocks = commentBlocks ctx comments
          concatMap commentBlockComments blocks `shouldBe` comments

  scenarioDir resourceDir $ \entry -> do
    let path = resourceDir </> entry
    if fileExtension path == Just ".hs"
      then do
        golden <- runIO (addExtension ".json" =<< addExtension ".golden" path)
        -- The comment facts themselves, through the codec they already have. A
        -- bespoke rendering would be a second format for the same data, with
        -- its own names for every style and attachment to keep in step with the
        -- codec's. This way there is one format, and the golden doubles as a
        -- test that the codec reads back what it wrote on real modules rather
        -- than only on generated ones, which is what goldenJSONValueFile
        -- asserts.
        it "attaches every comment to what the golden says" $
          goldenJSONValueFile (toFilePath golden) $
            moduleContextComments <$> factsForFile shippedRules path [] ComponentLib

        -- Attachment is anchored to structure rather than to layout, so
        -- reformatting must not move a comment from one subject to another, and
        -- must not lose one.
        --
        -- Only the subject is compared. Spans move, and ormolu also
        -- rewrites a trailing @-- ^@ on a record field into a leading @-- |@,
        -- which changes the comment without changing what it is about.
        unless (filename path `elem` formatSensitive) $
          it "attaches the same way after ormolu" $ do
            before' <- subjects <$> factsForFile shippedRules path [] ComponentLib
            original <- TIO.readFile (toFilePath path)
            formatted <-
              T.pack
                <$> readProcess "ormolu" ["--stdin-input-file", toFilePath path] (T.unpack original)
            after' <- subjects <$> factsForSource shippedRules path [] ComponentLib formatted
            after' `shouldBe` before'
      else
        -- Nothing else belongs here, so a stray file fails rather than sitting
        -- unread, and a golden whose module was deleted fails with it.
        it "is a golden belonging to a module beside it" $
          case T.stripSuffix ".golden.json" (T.pack (toFilePath (filename path))) of
            Nothing ->
              expectationFailure (unwords ["Neither a module nor its golden:", toFilePath path])
            Just moduleName -> do
              moduleFile <- (parent path </>) <$> parseRelFile (T.unpack moduleName)
              present <- forgivingAbsence (getModificationTime moduleFile)
              isJust present `shouldBe` True

-- | ormolu does not preserve what these comments are about, so hopinion cannot
-- either, and asserting that it does would be asserting something false.
--
-- A comment at the end of a @do@ block is moved to column zero with a blank
-- line above it, which is exactly the shape that means "attached to nothing".
-- The reformatting changes the meaning, so the attachment changing with it is
-- correct.
formatSensitive :: [Path Rel File]
formatSensitive = [[relfile|07-end-of-do-block.hs|]]

-- | What each comment is about, in order, with the positions left out.
subjects :: ModuleContext -> [Text]
subjects mf = map (subjectOf . commentFactAttachment) (moduleContextComments mf)

subjectOf :: Attachment -> Text
subjectOf a = case a of
  AttachedToDecl d -> T.concat ["decl ", declNameText d]
  AttachedToStatement d _ -> T.concat ["statement in ", declNameText d]
  AttachedToFile -> "file"
  AttachedToExportList -> "export list"
  Unattached -> "unattached"
