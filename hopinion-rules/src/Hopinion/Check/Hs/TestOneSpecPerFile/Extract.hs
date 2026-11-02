{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Hs.TestOneSpecPerFile.Extract (exportListOf) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Hs
import GHC.Types.SrcLoc (unLoc)
import Hopinion.Check.Hs.TestOneSpecPerFile.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Module
import Hopinion.Parse (ParsedModule (..))

-- | The export list, and the entries in it.
--
-- Documentation is not an entry: a section heading or a paragraph in an export
-- list exports nothing, so a list holding one and a name exports one thing.
exportListOf :: ModuleContext -> ParsedModule -> ExportList
exportListOf ctx parsed = case hsmodExports (unLoc (parsedModuleAst parsed)) of
  Nothing -> ExportListNone
  Just lies ->
    ExportList
      (spanOfSrcSpan (moduleContextPath ctx) (getLocA lies))
      (mapMaybe (exportedText . unLoc) (unLoc lies))

exportedText :: IE GhcPs -> Maybe Text
exportedText = \case
  IEVar _ n _ -> Just (wrappedText (unLoc n))
  IEThingAbs _ n _ -> Just (wrappedText (unLoc n))
  IEThingAll _ n _ -> Just (subordinates (wrappedText (unLoc n)) [".."])
  IEThingWith _ n wildcard ns _ ->
    Just
      ( subordinates
          (wrappedText (unLoc n))
          (case wildcard of NoIEWildcard -> map (wrappedText . unLoc) ns; IEWildcard _ -> [".."])
      )
  IEModuleContents _ name -> Just (T.pack (unwords ["module", moduleNameString (unLoc name)]))
  IEGroup {} -> Nothing
  IEDoc {} -> Nothing
  IEDocNamed {} -> Nothing

subordinates :: Text -> [Text] -> Text
subordinates name children = T.concat [name, "(", T.intercalate ", " children, ")"]

wrappedText :: IEWrappedName GhcPs -> Text
wrappedText = \case
  IEName _ n -> rdrText (unLoc n)
  IEDefault _ n -> adorned "default" n
  IEPattern _ n -> adorned "pattern" n
  IEType _ n -> adorned "type" n
  where
    adorned :: String -> LIdP GhcPs -> Text
    adorned keyword n = T.pack (unwords [keyword, T.unpack (rdrText (unLoc n))])
