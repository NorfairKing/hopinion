{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Where in the repository something is, and what it is about.
--
-- Two answers to two different questions, and every finding carries both. A
-- span is exact and is what a person is shown; a scope key is coarse, survives
-- the code around it being edited, and is what a suppression is matched on.
module Hopinion.Facts.Place
  ( Position (..),
    positionFromGhc,
    Span (..),
    spanContains,
    spanText,
    parseSpan,
    wholeFileSpan,
    isWholeFileSpan,
    ModuleRef (..),
    moduleRefText,
    parseModuleRef,
    ScopeKey (..),
    scopeKeyModule,
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Validity
import Data.Validity.Path ()
import Database.Persist (PersistField (..))
import Database.Persist.Sql (PersistFieldSql (..), SqlType (..))
import GHC.Generics (Generic)
import Hopinion.Facts.Name
import Path (File, Path, Rel, parseRelFile)

-- | 'Word' rather than 'Int', because half of an 'Int' is a line number that
-- cannot exist.
data Position = Position
  { positionLine :: !Word,
    positionCol :: !Word
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Position)

-- | Both are one-based, which the type cannot say on its own: a 'Word' rules
-- out the negative half and leaves zero. GHC counts from one, every renderer
-- counts from one, and a line zero has never been anything but a mistake.
instance Validity Position where
  validate p =
    mconcat
      [ genericValidate p,
        declare "the line is one-based" (positionLine p >= 1),
        declare "the column is one-based" (positionCol p >= 1)
      ]

-- | A position out of the numbers GHC counts with, which start at one.
--
-- Clamped rather than trusted, because this is the only place that has to know
-- they are one-based: a zero arriving here would otherwise be a line number
-- near two to the sixty-fourth rather than a place a reader can look at.
positionFromGhc :: Int -> Int -> Position
positionFromGhc line col =
  Position {positionLine = oneBased line, positionCol = oneBased col}
  where
    oneBased :: Int -> Word
    oneBased = fromIntegral . max 1

instance HasCodec Position where
  codec =
    object "Position" $
      Position
        <$> requiredField "line" "one-based line" .= positionLine
        <*> requiredField "col" "one-based column" .= positionCol

-- | A range in one file.
--
-- The file is on the span rather than on each end. Both ends are always in the
-- same file, so carrying it twice makes a disagreement representable without
-- making it mean anything, and it was most of what the fact files weighed.
data Span = Span
  { spanFile :: !(Path Rel File),
    spanStart :: !Position,
    spanEnd :: !Position
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Span)

instance Validity Span

-- | A span in one column, as @startLine:startCol-endLine:endCol file@.
--
-- One column per rule rather than five, and no conversion written out by hand
-- at each of them.
--
-- The numbers come first so that the rest of the text is the path, whatever is
-- in it. A path may hold a colon or a space; it may not hold a newline, and
-- nothing here needs it to.
spanText :: Span -> Text
spanText sp =
  T.concat
    [ T.pack (show (positionLine (spanStart sp))),
      ":",
      T.pack (show (positionCol (spanStart sp))),
      "-",
      T.pack (show (positionLine (spanEnd sp))),
      ":",
      T.pack (show (positionCol (spanEnd sp))),
      " ",
      relPathText (spanFile sp)
    ]

parseSpan :: Text -> Maybe Span
parseSpan t = do
  let (positions, rest) = T.breakOn " " t
  file <- parseRelFile (T.unpack (T.drop 1 rest))
  (start, end) <- twoOf "-" positions
  startPos <- position start
  endPos <- position end
  pure Span {spanFile = file, spanStart = startPos, spanEnd = endPos}
  where
    twoOf sep s = case T.splitOn sep s of
      [a, b] -> Just (a, b)
      _ -> Nothing

    position s = do
      (l, c) <- twoOf ":" s
      Position <$> number l <*> number c

    number s = case T.decimal s of
      Right (n, "") -> Just n
      _ -> Nothing

instance PersistField Span where
  toPersistValue = toPersistValue . spanText
  fromPersistValue v = do
    t <- fromPersistValue v
    maybe (Left "not a span") Right (parseSpan t)

instance PersistFieldSql Span where
  sqlType _ = SqlString

instance HasCodec Span where
  codec =
    object "Span" $
      Span
        <$> requiredFieldWith "file" relPathCodec "repo-relative path" .= spanFile
        <*> requiredField "start" "inclusive start" .= spanStart
        <*> requiredField "end" "exclusive end" .= spanEnd

-- | One span inside another, in the same file.
spanContains :: Span -> Span -> Bool
spanContains outer inner =
  spanFile outer == spanFile inner
    && spanStart outer <= spanStart inner
    && spanEnd inner <= spanEnd outer

-- | A whole file, for a finding whose subject is the file rather than anything
-- in it.
wholeFileSpan :: Path Rel File -> Span
wholeFileSpan rp =
  let start = Position {positionLine = 1, positionCol = 1}
   in Span {spanFile = rp, spanStart = start, spanEnd = start}

-- | Whether this span is a whole file rather than something in one.
--
-- Nothing in a file starts and ends in the same place, so the empty span is the
-- encoding and this is the name for it. Two things read it and would otherwise
-- each carry their own idea of what it means: the renderer, which widens such a
-- span to a line before it can underline anything, and the suppression, which
-- has to be file-scoped.
isWholeFileSpan :: Span -> Bool
isWholeFileSpan sp = spanStart sp == spanEnd sp

-- | Which module, in which component.
--
-- Both halves, because a module name is not an identity: every component with a
-- @main-is@ has one called @Main@, so a suppression written against @main@ in
-- one test suite would otherwise answer for a finding about @main@ in another.
data ModuleRef = ModuleRef
  { moduleRefComponent :: !ComponentName,
    moduleRefModule :: !ModuleKey
  }
  -- Read because persistent asks it of anything in a composite key.
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ModuleRef)

instance Validity ModuleRef

instance HasCodec ModuleRef where
  codec =
    object "ModuleRef" $
      ModuleRef
        <$> requiredField "component" "the component that claims it" .= moduleRefComponent
        <*> requiredField "module" "the module's own name" .= moduleRefModule

-- | A module's identity in one column, as @component:module@.
--
-- One column because a fact spread over two of them is one a query can compare
-- half of, and half of an identity lets one component's @Main@ answer for
-- another's. Written this way a join has nothing to compare but the whole.
--
-- The component comes first because neither name may hold a colon, so the first
-- one separates them.
moduleRefText :: ModuleRef -> Text
moduleRefText r =
  T.concat [componentNameText (moduleRefComponent r), ":", moduleKeyText (moduleRefModule r)]

parseModuleRef :: Text -> Maybe ModuleRef
parseModuleRef t = case T.breakOn ":" t of
  (_, rest) | T.null rest -> Nothing
  (component, rest) ->
    Just
      ModuleRef
        { moduleRefComponent = ComponentName component,
          moduleRefModule = ModuleKey (T.drop 1 rest)
        }

instance PersistField ModuleRef where
  toPersistValue = toPersistValue . moduleRefText
  fromPersistValue v = do
    t <- fromPersistValue v
    maybe (Left "not a module reference") Right (parseModuleRef t)

instance PersistFieldSql ModuleRef where
  sqlType _ = SqlString

-- | Coarse and portable across the fact boundary. Deliberately no
-- statement-level variant: a portable statement address would have to be an
-- index path into a syntax tree, which breaks under editing.
data ScopeKey
  = ScopeOfFile !ModuleRef
  | ScopeOfDecl !ModuleRef !DeclName
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ScopeKey)

instance Validity ScopeKey

scopeKeyModule :: ScopeKey -> ModuleRef
scopeKeyModule = \case
  ScopeOfFile m -> m
  ScopeOfDecl m _ -> m

instance HasCodec ScopeKey where
  codec =
    named "ScopeKey" $
      object "ScopeKey" $
        bimapCodec
          (\(m, mDecl) -> Right (maybe (ScopeOfFile m) (ScopeOfDecl m) mDecl))
          ( \case
              ScopeOfFile m -> (m, Nothing)
              ScopeOfDecl m d -> (m, Just d)
          )
          ( (,)
              <$> requiredField "module" "the module the scope is in" .= fst
              <*> optionalField "decl" "the declaration, absent for a whole file" .= snd
          )
