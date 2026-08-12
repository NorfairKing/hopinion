{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Extension resolution and the two passes over a module's text.
--
-- Comments, type applications and Template Haskell use come from the token
-- stream, because that is a flat, complete list with exact spans and no
-- dependence on where exact-print annotations chose to hang a comment.
-- Declarations and instances come from the parse tree, which is the only place
-- they exist.
module Hopinion.Parse
  ( ParseInput (..),
    ParsedModule (..),
    NameOccurrence (..),
    parseHaskellModule,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Data.FastString (FastString, mkFastString, unpackFS)
import qualified GHC.Data.StringBuffer as SB
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Session
  ( DynFlags,
    defaultDynFlags,
    xopt,
    xopt_set,
    xopt_unset,
  )
import GHC.Hs (GhcPs, HsModule)
import qualified GHC.LanguageExtensions as LangExt
import GHC.Parser.Lexer
  ( PState (..),
    ParseResult (..),
    Token (..),
    lexTokenStream,
  )
import GHC.Types.SrcLoc
  ( GenLocated (..),
    Located,
    RealSrcSpan,
    mkRealSrcLoc,
    srcSpanEndCol,
    srcSpanEndLine,
    srcSpanStartCol,
    srcSpanStartLine,
  )
import qualified GHC.Types.SrcLoc as SrcLoc
import Hopinion.Comment (RawComment (..))
import Hopinion.Facts
import qualified Language.Haskell.GhclibParserEx.GHC.Driver.Session as ExSession
import qualified Language.Haskell.GhclibParserEx.GHC.Parser as ExParser
import Language.Haskell.GhclibParserEx.GHC.Settings.Config (fakeSettings)
import Path (File, Path, Rel, toFilePath)

data ParseInput = ParseInput
  { -- | How the module is named in every fact and finding.
    parseInputRelPath :: !(Path Rel File),
    -- | @default-extensions@ from the cabal file, before in-file pragmas.
    parseInputDefaultExtensions :: ![Text],
    -- | The module's own text, as it is on disk.
    --
    -- Text rather than a String, so that the text the spans refer to is this
    -- same value whenever CPP has not rewritten it. The parser is what wants a
    -- String, so the parser is what makes one.
    parseInputSource :: !Text
  }

data ParsedModule = ParsedModule
  { parsedModuleAst :: !(Located (HsModule GhcPs)),
    -- | The text the spans in here refer to, which is the module's own source
    -- unless it uses CPP.
    parsedModuleSource :: !Text,
    parsedModuleComments :: ![RawComment],
    parsedModuleTypeApps :: ![TypeAppFact],
    parsedModuleTemplateHaskell :: !TemplateHaskellUse,
    -- | The line the @module@ keyword is on, if there is a header.
    parsedModuleHeaderLine :: !(Maybe Word),
    -- | The parenthesised export list, from its opening paren to its closing
    -- paren, which is what tells a legitimate section header from a stray one.
    parsedModuleExportListSpan :: !(Maybe Span),
    parsedModuleNames :: ![NameOccurrence]
  }

-- | A capitalised name the module writes, before anything has worked out which
-- declaration it is inside.
data NameOccurrence = NameOccurrence
  { nameOccurrenceText :: !Text,
    nameOccurrenceSpan :: !Span
  }

parseHaskellModule :: ParseInput -> IO (Either (Position, Text) ParsedModule)
parseHaskellModule input = do
  let rp = parseInputRelPath input
  let src = T.unpack (parseInputSource input)
  -- Only ever a name in a message: the source is handed over as text, so nothing
  -- here reads the file.
  let named = toFilePath rp
  eFlags <-
    ExSession.parsePragmasIntoDynFlags
      (baseDynFlags (parseInputDefaultExtensions input))
      ([], [])
      named
      src
  case eFlags of
    Left err -> pure (Left (startOfFile, T.pack err))
    Right flags -> do
      -- Blanked only when there is CPP, and when there is not, the text the
      -- spans refer to is the text that came in rather than a repacking of it.
      let preprocessed = if xopt LangExt.Cpp flags then Just (blankCppDirectives src) else Nothing
      let effective = fromMaybe src preprocessed
      let opts = initParserOpts flags
      let buf = SB.stringToStringBuffer effective
      let startLoc = mkRealSrcLoc (mkFastString named) 1 1
      case lexTokenStream opts buf startLoc of
        PFailed pst -> pure (Left (lexerPosition pst, "Failed to lex the module."))
        POk _ toks -> do
          let located = [(t, sp) | L s t <- toks, Just sp <- [toSpan rp s]]
          case ExParser.parseFile named flags effective of
            PFailed pst -> pure (Left (lexerPosition pst, "Failed to parse the module."))
            POk _ ast ->
              pure
                ( Right
                    ParsedModule
                      { parsedModuleAst = ast,
                        parsedModuleSource = maybe (parseInputSource input) T.pack preprocessed,
                        parsedModuleComments = commentsOf located,
                        parsedModuleTypeApps = typeAppsOf located,
                        parsedModuleTemplateHaskell = templateHaskellOf located,
                        parsedModuleHeaderLine = headerLineOf located,
                        parsedModuleExportListSpan = exportListSpanOf located,
                        parsedModuleNames = namesOf located
                      }
                )

-- | The bare parser configuration, plus the cabal file's @default-extensions@.
-- In-file @LANGUAGE@ pragmas are layered on top of this by the caller.
baseDynFlags :: [Text] -> DynFlags
baseDynFlags = foldl apply (defaultDynFlags fakeSettings)
  where
    apply :: DynFlags -> Text -> DynFlags
    apply flags name =
      case T.stripPrefix "No" name of
        Just rest
          | Just ext <- ExSession.readExtension (T.unpack rest) -> xopt_unset flags ext
        _ -> case ExSession.readExtension (T.unpack name) of
          Just ext -> xopt_set flags ext
          -- An extension the parser does not know is not a parse failure: it
          -- is either a typo in the cabal file or a flag with no syntactic
          -- effect, and neither is this tool's business.
          Nothing -> flags

-- | hopinion never runs a C preprocessor: it has no build, so it does not know
-- what @MIN_VERSION_foo@ expands to. Directive lines are blanked, keeping their
-- line numbers, which leaves every branch of a conditional in the parse.
--
-- That over-approximates: a declaration in a branch the build never compiles is
-- still seen. For a tool that reports what is in a file that is the right
-- direction to be wrong in, and it is what makes a CPP module analysable at all.
blankCppDirectives :: String -> String
blankCppDirectives = unlines . go . lines
  where
    go [] = []
    go (l : ls)
      | isDirective l =
          let (continuation, rest) = spanContinuation l ls
           in map (const "") (l : continuation) ++ go rest
      | otherwise = l : go ls

    spanContinuation :: String -> [String] -> ([String], [String])
    spanContinuation prev ls
      | endsWithBackslash prev = case ls of
          (next : rest) ->
            let (cs, r) = spanContinuation next rest
             in (next : cs, r)
          [] -> ([], [])
      | otherwise = ([], ls)

    endsWithBackslash l = case reverse l of
      ('\\' : _) -> True
      _ -> False

    isDirective s = case dropWhile (== ' ') s of
      ('#' : _) -> True
      _ -> False

startOfFile :: Position
startOfFile = Position {positionLine = 1, positionCol = 1}

lexerPosition :: PState -> Position
lexerPosition pst =
  let rsl = SrcLoc.psRealLoc (loc pst)
   in positionFromGhc (SrcLoc.srcLocLine rsl) (SrcLoc.srcLocCol rsl)

toSpan :: Path Rel File -> SrcLoc.SrcSpan -> Maybe Span
toSpan rp s = case s of
  SrcLoc.RealSrcSpan rss _ -> Just (realToSpan rp rss)
  SrcLoc.UnhelpfulSpan _ -> Nothing

realToSpan :: Path Rel File -> RealSrcSpan -> Span
realToSpan rp rss =
  Span
    { spanFile = rp,
      spanStart = positionFromGhc (srcSpanStartLine rss) (srcSpanStartCol rss),
      spanEnd = positionFromGhc (srcSpanEndLine rss) (srcSpanEndCol rss)
    }

commentsOf :: [(Token, Span)] -> [RawComment]
commentsOf toks =
  [ RawComment {rawCommentSpan = sp, rawCommentText = T.pack text}
  | (t, sp) <- toks,
    Just text <- [commentText t]
  ]

commentText :: Token -> Maybe String
commentText = \case
  ITlineComment s _ -> Just s
  ITblockComment s _ -> Just s
  _ -> Nothing

-- | Every capitalised name the module writes, which is every @conid@ token.
--
-- The token stream is what makes this answerable at all: the same word in a
-- comment or in a string literal is a different token, and a qualified name is
-- one token holding both halves, so a bare name here is one the code wrote and
-- not the tail of a module name.
namesOf :: [(Token, Span)] -> [NameOccurrence]
namesOf toks =
  [ NameOccurrence {nameOccurrenceText = fsText s, nameOccurrenceSpan = sp}
  | (ITconid s, sp) <- toks
  ]

-- | A splice or a quasiquote, and which, since the obligation engine reads the
-- two differently. GHC's whitespace-sensitive lexing separates the prefix @$@
-- of a splice from the @$@ operator, so this does not fire on ordinary
-- application.
templateHaskellOf :: [(Token, Span)] -> TemplateHaskellUse
templateHaskellOf toks
  | any (isSplice . fst) toks = UsesSplices
  | any (isQuasiQuote . fst) toks = UsesQuasiQuotes
  | otherwise = NoTemplateHaskell
  where
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

-- | @f \@T@ and @f \@(T a b)@, reported as the function's name and the head
-- type constructor with applied arguments dropped.
typeAppsOf :: [(Token, Span)] -> [TypeAppFact]
typeAppsOf = go
  where
    go ((fnTok, fnSpan) : (ITtypeApp, _) : rest)
      | Just fn <- varName fnTok,
        Just headName <- headTypeOf rest =
          TypeAppFact
            { typeAppFactFunction = fn,
              typeAppFactHead = TypeHead headName,
              typeAppFactSpan = fnSpan
            }
            : go rest
    go (_ : rest) = go rest
    go [] = []

    headTypeOf ((IToparen, _) : rest) = headTypeOf rest
    headTypeOf ((t, _) : _) = conName t
    headTypeOf [] = Nothing

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

headerLineOf :: [(Token, Span)] -> Maybe Word
headerLineOf toks = case [sp | (ITmodule, sp) <- toks] of
  (sp : _) -> Just (positionLine (spanStart sp))
  [] -> Nothing

-- | From the first paren after the module header to its match. Taken from the
-- tokens rather than from the export list's own span so that a comment before
-- the first export still falls inside it.
exportListSpanOf :: [(Token, Span)] -> Maybe Span
exportListSpanOf toks =
  case drop 1 (dropWhile (not . isModule . fst) toks) of
    [] -> Nothing
    afterModule ->
      case dropWhile (not . isOpen . fst) (takeWhile (not . isWhere . fst) afterModule) of
        [] -> Nothing
        ((_, openSpan) : inside) ->
          case closeOf (0 :: Word) inside of
            Nothing -> Nothing
            Just closeSpan -> Just openSpan {spanEnd = spanEnd closeSpan}
  where
    isModule = \case
      ITmodule -> True
      _ -> False
    isWhere = \case
      ITwhere -> True
      _ -> False
    isOpen = \case
      IToparen -> True
      _ -> False
    closeOf _ [] = Nothing
    closeOf depth ((t, sp) : rest) = case t of
      IToparen -> closeOf (depth + 1) rest
      ITcparen -> if depth == 0 then Just sp else closeOf (depth - 1) rest
      _ -> closeOf depth rest
