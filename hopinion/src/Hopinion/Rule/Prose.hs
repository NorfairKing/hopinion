{-# LANGUAGE TemplateHaskell #-}

-- | A rule's prose, kept in text files beside the rule rather than in string
-- literals, so that it is written and reviewed as prose.
module Hopinion.Rule.Prose (proseBeside) where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import qualified Data.Text as T
import Language.Haskell.TH (Exp, Q, loc_filename, location)
import Path (SomeBase (..), fromAbsFile, fromRelFile, parent, parseRelFile, parseSomeFile, (</>))

-- | The text of a file in the directory of the module this is spliced into,
-- without the whitespace around it.
--
-- Embedding makes the file a dependency of that module, so editing the prose
-- rebuilds the rule.
-- A name that is not a relative file is a compile error rather than a rule
-- with no text.
proseBeside :: String -> Q Exp
proseBeside name = do
  here <- location
  file <- case (,) <$> parseSomeFile (loc_filename here) <*> parseRelFile name of
    Left err -> fail (unwords ["No prose file", name, "beside", loc_filename here, concat ["(", show err, ")"]])
    Right (Abs moduleFile, beside) -> pure (fromAbsFile (parent moduleFile </> beside))
    Right (Rel moduleFile, beside) -> makeRelativeToProject (fromRelFile (parent moduleFile </> beside))
  [|T.strip $(embedStringFile file)|]
