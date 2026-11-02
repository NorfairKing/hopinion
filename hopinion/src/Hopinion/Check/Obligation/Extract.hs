{-# LANGUAGE LambdaCase #-}

-- | Reading the obligation family's facts off a module's tokens.
--
-- The tokens rather than the parse tree, because GHC's whitespace-sensitive
-- lexing has already told a type application from an at-pattern and a prefix
-- splice from the @$@ operator.
module Hopinion.Check.Obligation.Extract
  ( typeAppsOf,
    templateHaskellOf,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Data.FastString (FastString, unpackFS)
import GHC.Parser.Lexer (Token (..))
import Hopinion.Check.Obligation.Fact
import Hopinion.Facts.Name
import Hopinion.Parse (ParsedModule (..), SourceToken (..))

-- | @f \@T@ and @f \@(T a b)@, reported as the function's name and the head
-- type constructor with applied arguments dropped.
typeAppsOf :: ParsedModule -> [TypeAppFact]
typeAppsOf = go . parsedModuleTokens
  where
    go :: [SourceToken] -> [TypeAppFact]
    go = \case
      (SourceToken fnTok fnSpan : SourceToken ITtypeApp _ : rest)
        | Just fn <- varName fnTok,
          Just headName <- headTypeOf rest ->
            TypeAppFact
              { typeAppFactFunction = fn,
                typeAppFactHead = TypeHead headName,
                typeAppFactSpan = fnSpan
              }
              : go rest
      (_ : rest) -> go rest
      [] -> []

    headTypeOf :: [SourceToken] -> Maybe Text
    headTypeOf = \case
      (SourceToken IToparen _ : rest) -> headTypeOf rest
      (SourceToken t _ : _) -> conName t
      [] -> Nothing

-- | A splice or a quasiquote, and which, since the obligation engine reads the
-- two differently.
templateHaskellOf :: ParsedModule -> TemplateHaskellUse
templateHaskellOf parsed
  | any (isSplice . sourceTokenToken) toks = TemplateHaskellUseSplices
  | any (isQuasiQuote . sourceTokenToken) toks = TemplateHaskellUseQuasiQuotes
  | otherwise = TemplateHaskellUseNone
  where
    toks = parsedModuleTokens parsed

    isSplice :: Token -> Bool
    isSplice = \case
      ITdollar -> True
      ITdollardollar -> True
      _ -> False

    isQuasiQuote :: Token -> Bool
    isQuasiQuote = \case
      ITquasiQuote _ -> True
      ITqQuasiQuote _ -> True
      _ -> False

varName :: Token -> Maybe Text
varName = \case
  ITvarid f -> Just (fsText f)
  ITqvarid (_, f) -> Just (fsText f)
  _ -> Nothing

conName :: Token -> Maybe Text
conName = \case
  ITconid f -> Just (fsText f)
  ITqconid (_, f) -> Just (fsText f)
  _ -> Nothing

fsText :: FastString -> Text
fsText = T.pack . unpackFS
