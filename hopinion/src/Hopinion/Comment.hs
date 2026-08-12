{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What a comment is, and what it is about.
--
-- Attachment is computed here in an explicit pass over spans rather than taken
-- from where exact-print annotations put a comment: that placement answers
-- "where must this be reprinted", not "what is this about", and the two differ
-- exactly where it matters. Every comment rule and the whole annotation
-- mechanism stand on it, and it fails silently when wrong.
--
-- The most consequential decision: a blank line between a comment and the code
-- below it means the comment is not attached to that code.
module Hopinion.Comment
  ( CommentStyle (..),
    Attachment (..),
    CommentFact (..),
    RawComment (..),
    CommentContext (..),
    mkCommentContext,
    CommentBlock (..),
    commentBlocks,
    attachComments,
    commentStyleOf,
    commentBody,
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Text ()
import GHC.Generics (Generic)
import Hopinion.Facts.Decl
import Hopinion.Facts.Name
import Hopinion.Facts.Place

data CommentStyle
  = StyleLine
  | StyleBlock
  | StyleHaddockNext
  | StyleHaddockPrev
  | StyleHaddockNamed
  | StylePragma
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec CommentStyle)

instance Validity CommentStyle

instance HasCodec CommentStyle where
  codec =
    named "CommentStyle" $
      stringConstCodec
        ( (StyleLine, "line")
            :| [ (StyleBlock, "block"),
                 (StyleHaddockNext, "haddock-next"),
                 (StyleHaddockPrev, "haddock-prev"),
                 (StyleHaddockNamed, "haddock-named"),
                 (StylePragma, "pragma")
               ]
        )

-- | 'Unattached' is an outcome rather than a failure: such a comment is ignored
-- by every rule that needs a subject, and such an annotation is an error.
--
-- 'AttachedToStatement' carries the enclosing declaration too, because the
-- portable scope key is only as fine as a declaration and would otherwise not
-- be recoverable for a comment inside one.
data Attachment
  = AttachedToDecl !DeclName
  | AttachedToStatement !DeclName !Span
  | AttachedToFile
  | AttachedToExportList
  | Unattached
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Attachment)

instance Validity Attachment

-- | A string for the three that are only themselves, an object for the two
-- that carry what they are about. The two shapes cannot be confused, which is
-- what makes the choice disjoint.
instance HasCodec Attachment where
  codec =
    named "Attachment" $
      dimapCodec fromEither toEither $
        disjointEitherCodec
          ( stringConstCodec
              ( (AttachedToFile, "file")
                  :| [(AttachedToExportList, "export-list"), (Unattached, "unattached")]
              )
          )
          ( object "AttachedTo" $
              (,)
                <$> requiredField "decl" "the declaration it is about" .= fst
                <*> optionalField "statement" "the statement it is about, when it is one" .= snd
          )
    where
      fromEither = either id (\(d, mSpan) -> maybe (AttachedToDecl d) (AttachedToStatement d) mSpan)
      toEither = \case
        AttachedToDecl d -> Right (d, Nothing)
        AttachedToStatement d s -> Right (d, Just s)
        other -> Left other

data CommentFact = CommentFact
  { commentFactSpan :: !Span,
    commentFactStyle :: !CommentStyle,
    commentFactText :: !Text,
    commentFactAttachment :: !Attachment
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec CommentFact)

instance Validity CommentFact

instance HasCodec CommentFact where
  codec =
    object "CommentFact" $
      CommentFact
        <$> requiredField "span" "where the comment block is" .= commentFactSpan
        <*> requiredField "style" "what sort of comment it is" .= commentFactStyle
        <*> requiredField "text" "the block's text, comment markers stripped" .= commentFactText
        <*> requiredField "attachment" "what it is about" .= commentFactAttachment

data RawComment = RawComment
  { rawCommentSpan :: !Span,
    rawCommentText :: !Text
  }
  deriving stock (Show, Eq, Generic)

instance Validity RawComment

data CommentContext = CommentContext
  { -- | Source lines by one-based line number. A map rather than a list
    -- because attachment looks lines up by number far more often than it walks
    -- them.
    commentContextLines :: !(Map Word Text),
    -- | The lines that hold nothing but comment. Taken from the comment spans
    -- rather than guessed from the text, since the continuation lines of a
    -- block comment start with neither @--@ nor @{-@ and would read as code.
    commentContextCommentOnly :: !(Set Word),
    -- | Top-level declarations in source order.
    commentContextDecls :: ![DeclFact],
    commentContextExportList :: !(Maybe Span),
    commentContextModuleHeaderLine :: !(Maybe Word)
  }
  deriving stock (Show, Eq, Generic)

instance Validity CommentContext

mkCommentContext :: [Text] -> [RawComment] -> [DeclFact] -> Maybe Span -> Maybe Word -> CommentContext
mkCommentContext ls comments decls mExportList mHeaderLine =
  let lineMap = M.fromList (zip [1 ..] ls)
   in CommentContext
        { commentContextLines = lineMap,
          commentContextCommentOnly = S.unions (map (commentOnlyLines lineMap) comments),
          commentContextDecls = decls,
          commentContextExportList = mExportList,
          commentContextModuleHeaderLine = mHeaderLine
        }

-- | The lines one comment leaves with nothing else on them.
--
-- Its first line counts only when nothing precedes the comment on it, which is
-- what keeps a trailing comment from making its line look comment-only. Its
-- last line counts only when nothing follows.
commentOnlyLines :: Map Word Text -> RawComment -> Set Word
commentOnlyLines lineMap rc =
  let sp = rawCommentSpan rc
      firstLine = positionLine (spanStart sp)
      lastLine = positionLine (spanEnd sp)
      -- Converted before the subtraction, not after: Text counts with 'Int', and
      -- a column of zero taken from would be a length rather than nothing.
      before = T.take (fromIntegral (positionCol (spanStart sp)) - 1) (lineOf firstLine)
      after = T.drop (fromIntegral (positionCol (spanEnd sp)) - 1) (lineOf lastLine)
      lineOf n = M.findWithDefault "" n lineMap
      -- Clamped to the file. A span is only ever as long as the module it came
      -- from, but building a set from the span's own arithmetic would make the
      -- cost of this depend on the numbers in it rather than on the file.
      lastOfFile = maybe 0 fst (M.lookupMax lineMap)
      interior = S.fromList [max 1 (firstLine + 1) .. min (lastLine - 1) lastOfFile]
      withFirst = if T.null (T.strip before) then S.insert firstLine else id
      withLast = if T.null (T.strip after) then S.insert lastLine else id
   in if firstLine == lastLine
        then
          if T.null (T.strip before) && T.null (T.strip after)
            then S.singleton firstLine
            else S.empty
        else withFirst (withLast interior)

-- | A maximal run of comment-only lines at one indentation with no blank
-- source line intervening. A trailing comment is always its own block, because
-- it belongs to the code on its own line.
data CommentBlock = CommentBlock
  { commentBlockComments :: ![RawComment],
    commentBlockSpan :: !Span,
    commentBlockStyle :: !CommentStyle,
    commentBlockText :: !Text,
    commentBlockTrailing :: !Trailing
  }
  deriving stock (Show, Eq, Generic)

instance Validity CommentBlock

data Trailing
  = OnItsOwnLines
  | TrailingCode
  deriving stock (Show, Eq, Generic)

instance Validity Trailing

commentStyleOf :: Text -> CommentStyle
commentStyleOf t
  | T.isPrefixOf "{-#" t = StylePragma
  | T.isPrefixOf "{-" t = StyleBlock
  | otherwise = fst (lineComment t)

-- | The prose inside a comment, with its markers removed.
commentBody :: Text -> Text
commentBody t
  | T.isPrefixOf "{-#" t = T.strip (dropEnd "#-}" (T.drop 3 t))
  | T.isPrefixOf "{-" t = T.strip (dropEnd "-}" (T.drop 2 t))
  | otherwise = T.strip (snd (lineComment t))
  where
    dropEnd suffix s = if T.isSuffixOf suffix s then T.dropEnd (T.length suffix) s else s

-- | What a line comment's marker says it is, and the prose after that marker.
--
-- One function rather than two, so that the style and the body cannot disagree
-- about where the marker ends. A body that kept a marker in its prose would be
-- read as prose by every rule and would stop
-- 'Hopinion.Annotation.parseAnnotation' recognising a suppression at all.
lineComment :: Text -> (CommentStyle, Text)
lineComment t =
  case [(style, rest) | (style, marker) <- markers, Just rest <- [T.stripPrefix marker t]] of
    ((style, rest) : _) -> (style, rest)
    -- Not a marker, so every leading dash is a dash: @--- foo@ and a row of
    -- them are prose, and a banner keeps whatever it is made of.
    [] -> (StyleLine, T.dropWhile (== '-') t)
  where
    -- Both spellings of each, since Haddock accepts the space and people write
    -- it both ways. Longest first, so the space is consumed by the marker
    -- rather than left at the front of the prose.
    markers =
      [ (StyleHaddockNext, "-- |"),
        (StyleHaddockNext, "--|"),
        (StyleHaddockPrev, "-- ^"),
        (StyleHaddockPrev, "--^"),
        (StyleHaddockNamed, "-- $"),
        (StyleHaddockNamed, "--$")
      ]

commentBlocks :: CommentContext -> [RawComment] -> [CommentBlock]
commentBlocks ctx = map toBlock . group . map withTrailing
  where
    withTrailing :: RawComment -> (RawComment, Trailing)
    withTrailing rc = (rc, if hasCodeBefore ctx rc then TrailingCode else OnItsOwnLines)

    -- A block is non-empty by construction, which is what lets its span and its
    -- style be read off without a partial function or a made-up fallback.
    group :: [(RawComment, Trailing)] -> [NonEmpty (RawComment, Trailing)]
    group [] = []
    group (x : xs) = go x [] xs
      where
        go first acc [] = [first :| reverse acc]
        go first acc (y : ys)
          | continues first (latest first acc) y = go first (y : acc) ys
          | otherwise = (first :| reverse acc) : go y [] ys

        latest first acc = case acc of
          (a : _) -> a
          [] -> first

    continues ::
      (RawComment, Trailing) ->
      (RawComment, Trailing) ->
      (RawComment, Trailing) ->
      Bool
    continues (firstC, _) (prevC, prevT) (nextC, nextT) =
      prevT == OnItsOwnLines
        && nextT == OnItsOwnLines
        && positionLine (spanEnd (rawCommentSpan prevC)) + 1 == positionLine (spanStart (rawCommentSpan nextC))
        && positionCol (spanStart (rawCommentSpan prevC)) == positionCol (spanStart (rawCommentSpan nextC))
        && commentStyleOf (rawCommentText prevC) == commentStyleOf (rawCommentText nextC)
        && not (isSuppression (rawCommentText nextC))
        && not (endsSuppression firstC prevC)

    -- A suppression's marker starts a comment, so a suppression written under
    -- the comment it is about is not swallowed into that comment.
    isSuppression t = T.isPrefixOf "[allow" (T.stripStart (commentBody t))

    -- And an empty line ends one, so a suppression's reason has a terminator
    -- and the comment written under it stays a comment that rules can still
    -- see. Ordinary prose keeps its paragraph breaks: an empty line only ends
    -- something when what it is ending is a suppression.
    endsSuppression firstC prevC =
      isSuppression (rawCommentText firstC) && T.null (commentBody (rawCommentText prevC))

    toBlock :: NonEmpty (RawComment, Trailing) -> CommentBlock
    toBlock grouped@((firstComment, trailing) :| _) =
      let cs = map fst (NE.toList grouped)
          start = spanStart (rawCommentSpan firstComment)
       in CommentBlock
            { commentBlockComments = cs,
              commentBlockSpan =
                Span
                  { spanFile = spanFile (rawCommentSpan firstComment),
                    spanStart = start,
                    spanEnd = foldl (\acc rc -> max acc (spanEnd (rawCommentSpan rc))) start cs
                  },
              commentBlockStyle = commentStyleOf (rawCommentText firstComment),
              commentBlockText = T.intercalate "\n" (map (commentBody . rawCommentText) cs),
              commentBlockTrailing = trailing
            }

hasCodeBefore :: CommentContext -> RawComment -> Bool
hasCodeBefore ctx rc =
  let l = positionLine (spanStart (rawCommentSpan rc))
      c = positionCol (spanStart (rawCommentSpan rc))
   in case lineAt ctx l of
        Nothing -> False
        Just line -> not (T.null (T.strip (T.take (fromIntegral c - 1) line)))

lineAt :: CommentContext -> Word -> Maybe Text
lineAt ctx n = M.lookup n (commentContextLines ctx)

lastLineNumber :: CommentContext -> Word
lastLineNumber ctx = maybe 0 fst (M.lookupMax (commentContextLines ctx))

-- | What each comment is about, first match wins.
attachComments :: CommentContext -> [RawComment] -> [CommentFact]
attachComments ctx rcs = map toFact (commentBlocks ctx rcs)
  where
    toFact :: CommentBlock -> CommentFact
    toFact b =
      CommentFact
        { commentFactSpan = commentBlockSpan b,
          commentFactStyle = commentBlockStyle b,
          commentFactText = commentBlockText b,
          commentFactAttachment = attachmentOf b
        }

    attachmentOf :: CommentBlock -> Attachment
    attachmentOf b
      | commentBlockStyle b == StylePragma = Unattached
      -- The export list is tested before trailing, which is the one place this
      -- order matters. As ormolu formats an export list, a section
      -- header shares its line with the opening paren, so the trailing test
      -- would claim it first. Nothing is lost: an export list holds no
      -- statement and no declaration for a trailing comment to attach to.
      | insideExportList b = AttachedToExportList
      | commentBlockTrailing b == TrailingCode = trailingAttachment b
      | beforeModuleHeader b = AttachedToFile
      | otherwise = case nextCodeLine ctx (endLine b) of
          Just next
            | not (blankBetween ctx (endLine b) next) ->
                case topLevelDeclStartingAt ctx next of
                  Just d -> AttachedToDecl (declFactName d)
                  Nothing -> case enclosingDecl ctx next of
                    Just d -> AttachedToStatement (declFactName d) (statementSpanAt ctx d next)
                    Nothing -> postfixOrUnattached b
          _ -> postfixOrUnattached b

    -- A trailing comment belongs to the code on its own line: to the whole
    -- declaration when that line is a declaration on its own, and to the
    -- statement otherwise.
    trailingAttachment :: CommentBlock -> Attachment
    trailingAttachment b =
      let l = startLine b
       in case topLevelDeclStartingAt ctx l of
            Just d | declFactSpan d `endsOnLine` l -> AttachedToDecl (declFactName d)
            _ -> case enclosingDecl ctx l of
              Just d -> AttachedToStatement (declFactName d) (statementSpanAt ctx d l)
              Nothing -> Unattached

    -- Postfix Haddock refers to what is above it. Anything else inside a
    -- declaration with nothing below it still belongs to the statement it sits
    -- in, which is the end-of-a-do-block case.
    postfixOrUnattached :: CommentBlock -> Attachment
    postfixOrUnattached b =
      case enclosingDecl ctx (startLine b) of
        Just d
          | commentBlockStyle b == StyleHaddockPrev -> AttachedToDecl (declFactName d)
          | otherwise -> case previousCodeLine ctx (startLine b) of
              Just prev -> AttachedToStatement (declFactName d) (statementSpanAt ctx d prev)
              Nothing -> AttachedToStatement (declFactName d) (declFactSpan d)
        Nothing ->
          if commentBlockStyle b == StyleHaddockPrev
            then case declEndingBefore ctx (startLine b) of
              Just d -> AttachedToDecl (declFactName d)
              Nothing -> Unattached
            else Unattached

    insideExportList :: CommentBlock -> Bool
    insideExportList b = case commentContextExportList ctx of
      Nothing -> False
      Just sp -> spanContains sp (commentBlockSpan b)

    beforeModuleHeader :: CommentBlock -> Bool
    beforeModuleHeader b = case commentContextModuleHeaderLine ctx of
      Nothing -> False
      Just h -> endLine b < h

    startLine b = positionLine (spanStart (commentBlockSpan b))
    endLine b = positionLine (spanEnd (commentBlockSpan b))

endsOnLine :: Span -> Word -> Bool
endsOnLine sp l = positionLine (spanStart sp) == l && positionLine (spanEnd sp) <= l

-- | The first line after @from@ that carries code. Comment-only lines are
-- skipped, since another comment block between a comment and its subject does
-- not separate them.
nextCodeLine :: CommentContext -> Word -> Maybe Word
nextCodeLine ctx from =
  case [l | l <- [from + 1 .. lastLineNumber ctx], isCodeLine ctx l] of
    (l : _) -> Just l
    [] -> Nothing

-- | Walked from the line rather than filtered from the file: the answer is
-- almost always the line above, and building the list costs the whole file.
--
-- The first line has nothing above it, said before the subtraction rather than
-- after: these are 'Word's, so one taken from zero is not a number below one but
-- a number above every line in the file, and the walk would never reach the
-- floor.
previousCodeLine :: CommentContext -> Word -> Maybe Word
previousCodeLine ctx from
  | from <= 1 = Nothing
  | otherwise = go (from - 1)
  where
    go l
      | l < 1 = Nothing
      | isCodeLine ctx l = Just l
      | otherwise = go (l - 1)

-- | A line carries code when it is neither blank nor comment-only.
isCodeLine :: CommentContext -> Word -> Bool
isCodeLine ctx l = case lineAt ctx l of
  Nothing -> False
  Just line ->
    not (T.null (T.strip line))
      && not (S.member l (commentContextCommentOnly ctx))

-- | Whether a blank source line falls strictly between two lines.
--
-- Bounded by 'takeWhile' rather than by @to - 1@: these are 'Word's, and one
-- taken from zero would ask about every line up to the largest there is.
blankBetween :: CommentContext -> Word -> Word -> Bool
blankBetween ctx from to = any isBlank (takeWhile (< to) [from + 1 ..])
  where
    isBlank l = maybe True (T.null . T.strip) (lineAt ctx l)

topLevelDeclStartingAt :: CommentContext -> Word -> Maybe DeclFact
topLevelDeclStartingAt ctx l =
  case [d | d <- commentContextDecls ctx, positionLine (spanStart (declFactSpan d)) == l] of
    (d : _) -> Just d
    [] -> Nothing

-- | The declaration a line is inside.
--
-- A declaration's span ends at its last token, so a comment at the end of a
-- @do@ block falls outside it. Indentation is what a person reads there, so a
-- line indented past a declaration that started above it, with no other
-- declaration in between, counts as inside that declaration.
enclosingDecl :: CommentContext -> Word -> Maybe DeclFact
enclosingDecl ctx l =
  case [d | d <- commentContextDecls ctx, spansLine d] of
    (d : _) -> Just d
    [] -> case reverse [d | d <- commentContextDecls ctx, startsAtOrBefore d] of
      (d : _) | trailsInto d -> Just d
      _ -> Nothing
  where
    spansLine d =
      positionLine (spanStart (declFactSpan d)) <= l
        && l <= positionLine (spanEnd (declFactSpan d))

    startsAtOrBefore d = positionLine (spanStart (declFactSpan d)) <= l

    -- Only the last declaration starting at or before the line can be the one
    -- this line trails into: top-level declarations do not overlap and are in
    -- source order, so any earlier one has that later declaration starting
    -- between its end and this line.
    trailsInto d =
      positionLine (spanEnd (declFactSpan d)) < l
        && indentOf ctx l >= positionCol (spanStart (declFactSpan d))

declEndingBefore :: CommentContext -> Word -> Maybe DeclFact
declEndingBefore ctx l =
  case reverse [d | d <- commentContextDecls ctx, positionLine (spanEnd (declFactSpan d)) < l] of
    (d : _) -> Just d
    [] -> Nothing

-- | The statement beginning at a line, taken to run until the layout ends: the
-- last following line that is blank or indented further than the first. Layout
-- is what a person means by "this statement", and it keeps attachment anchored
-- to structure rather than to a syntax tree path that editing invalidates.
statementSpanAt :: CommentContext -> DeclFact -> Word -> Span
statementSpanAt ctx d l =
  let file = spanFile (declFactSpan d)
      declEnd = positionLine (spanEnd (declFactSpan d))
      indent = indentOf ctx l
      lastLine = go l (l + 1)
      go acc n
        | n > declEnd = acc
        | otherwise = case lineAt ctx n of
            Nothing -> acc
            Just line
              | T.null (T.strip line) -> go acc (n + 1)
              | indentOf ctx n > indent -> go n (n + 1)
              | otherwise -> acc
      endCol = maybe 1 ((+ 1) . fromIntegral . T.length) (lineAt ctx lastLine)
   in Span
        { spanFile = file,
          spanStart = Position {positionLine = firstLine, positionCol = 1},
          spanEnd = Position {positionLine = lastLine, positionCol = endCol}
        }
  where
    -- The statement starts at the top of the comment run above it, because a
    -- statement's comments are part of what a suppression about that statement
    -- is about: otherwise an annotation could not suppress a finding about the
    -- comment right above it, which is exactly where such a comment sits.
    --
    -- Which lines are comment comes from the spans rather than from what the
    -- text starts with, since the continuation lines of a block comment start
    -- with neither dashes nor a brace.
    firstLine = climb l
    -- Stops at the first line for the reason 'previousCodeLine' does: one taken
    -- from zero is not a line above the first.
    climb n = if n > 1 && S.member (n - 1) (commentContextCommentOnly ctx) then climb (n - 1) else n

indentOf :: CommentContext -> Word -> Word
indentOf ctx l = maybe 0 (fromIntegral . T.length . T.takeWhile (== ' ')) (lineAt ctx l)
