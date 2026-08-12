{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What a person reads: a report drawn against the code it points at.
--
-- Separate from the report itself because rendering needs the source and
-- judging does not: a command that reads a report file to decide whether it is
-- clean has no source to hand. 'SourceMap' makes that dependency explicit, so a
-- caller who cannot supply the source says so rather than silently rendering
-- reports with nothing under them.
module Hopinion.Report.Render
  ( SourceMap (..),
    renderReport,
    renderReportColoured,
    printReport,
    writeReportTo,
    sourcesForReport,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import Data.Either (lefts, rights)
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Validity.Containers ()
import qualified Error.Diagnose as D
import GHC.Generics (Generic)
import Hopinion.Annotation (OverBroad (..), suppressionFor, suppressionIsFileScoped)
import Hopinion.Facts
import Hopinion.Project (SourceRoot, readSource, sourceFileIn)
import Hopinion.Report
import Hopinion.Rule
import Hopinion.Rule.Id
import Path (Abs, Dir, File, Path, Rel, toFilePath, (</>))
import Path.IO (ensureDir, forgivingAbsence)
import Prettyprinter (Doc, SimpleDocStream, defaultLayoutOptions, layoutPretty, reAnnotate, unAnnotate)
import qualified Prettyprinter.Render.Terminal as Ansi
import qualified Prettyprinter.Render.Text as Plain
import System.IO (Handle, hIsTerminalDevice)

-- | The text of every file a report points into, which is what turns a
-- position into something a reader can look at.
newtype SourceMap = SourceMap (M.Map (Path Rel File) Text)
  deriving stock (Show, Eq, Generic)

-- | The files a caller has to read for 'renderReport' to have anything to show.
reportFiles :: Complaints -> [Path Rel File]
reportFiles report =
  nub (mapMaybe (fmap spanFile . complaintSpan) (complaintList report))

-- | Where a complaint is, when it is anywhere. A failure is the one that is
-- not: there is no code to point at.
complaintSpan :: Complaint -> Maybe Span
complaintSpan = \case
  ComplaintFinding f -> Just (findingSpan f)
  ComplaintUnused a -> Just (annotationFactSpan a)
  ComplaintOverBroad o -> Just (annotationFactSpan (overBroadAnnotation o))
  ComplaintProblem p -> Just (annotationProblemSpan p)
  ComplaintFailure _ -> Nothing

-- | Deterministic and free of escape sequences, which is both what a test can
-- assert on and what belongs in a build log.
renderReport :: RuleSet -> SourceMap -> Complaints -> Text
renderReport = renderedWith Plain.renderStrict unAnnotate

-- | The same report with diagnose's own colours, which is what a person at a
-- terminal is shown.
--
-- Text rather than a write to a handle, so that the coloured rendering is a
-- value a test can look at. Nothing else asserts that this path is coloured,
-- and a run in a terminal is the only place the difference shows.
renderReportColoured :: RuleSet -> SourceMap -> Complaints -> Text
renderReportColoured = renderedWith Ansi.renderStrict (reAnnotate D.defaultStyle)

-- | One document, laid out once, rendered by whichever of the two the caller
-- asked for. Shared so that the colour is the only difference between them.
--
-- No newline is added: diagnose's document ends with one, and appending a
-- second put a blank line at the end of every report a caller captured while a
-- caller at a terminal got none.
renderedWith ::
  (SimpleDocStream ann -> Text) ->
  (Doc (D.Annotation Ansi.AnsiStyle) -> Doc ann) ->
  RuleSet ->
  SourceMap ->
  Complaints ->
  Text
renderedWith render restyle rs sources report
  | isClean report = ""
  | otherwise =
      render
        ( layoutPretty
            defaultLayoutOptions
            (restyle (D.prettyDiagnostic D.WithUnicode (D.TabSize 2) (diagnosticFor rs sources report)))
        )

-- | Coloured when a person is looking, plain when the output is being
-- captured. Dropping the colour is not enough: the styled renderer still emits
-- a reset sequence around every character.
printReport :: RuleSet -> Handle -> SourceMap -> Complaints -> IO ()
printReport rs handle sources report
  | isClean report = pure ()
  | otherwise = do
      isTerminal <- hIsTerminalDevice handle
      let render = if isTerminal then renderReportColoured else renderReport
      TIO.hPutStr handle (render rs sources report)

diagnosticFor :: RuleSet -> SourceMap -> Complaints -> D.Diagnostic String
diagnosticFor rs sm@(SourceMap sources) report =
  foldl' D.addReport withFiles (reportsIn rs sm report)
  where
    withFiles =
      foldl'
        (\d (rp, contents) -> D.addFile d (T.unpack (relPathText rp)) (T.unpack contents))
        mempty
        (M.toList sources)

-- | Everything the run has to say, in the order a reader wants it: what the
-- tool could not do, then what is wrong with the code, then what is wrong with
-- the suppressions written about it.
reportsIn :: RuleSet -> SourceMap -> Complaints -> [D.Report String]
reportsIn rs sources report = map (complaintReport rs sources) (sortOn ordering (complaintList report))
  where
    -- Findings among themselves are in source order, which is how a reader
    -- walks a file.
    ordering :: Complaint -> (Word, Maybe Span)
    ordering c = case c of
      ComplaintFailure _ -> (0, Nothing)
      ComplaintProblem _ -> (1, complaintSpan c)
      ComplaintFinding _ -> (2, complaintSpan c)
      ComplaintUnused _ -> (3, complaintSpan c)
      ComplaintOverBroad _ -> (4, complaintSpan c)

complaintReport :: RuleSet -> SourceMap -> Complaint -> D.Report String
complaintReport rs sources = \case
  ComplaintFailure f -> failureReport (renderFailure f)
  ComplaintProblem p -> problemReport sources p
  ComplaintFinding f -> findingReport rs sources f
  ComplaintUnused a -> unusedReport sources a
  ComplaintOverBroad o -> overBroadReport sources o

-- | No marker, for the reason 'Hopinion.Report.Complaint' gives: there is no
-- code to point at.
failureReport :: Text -> D.Report String
failureReport t = D.Err (Just "HOPINION_FAILED") (T.unpack t) [] []

problemReport :: SourceMap -> AnnotationProblem -> D.Report String
problemReport sources p =
  D.Err
    (Just "BROKEN_SUPPRESSION")
    (T.unpack (annotationProblemMessage p))
    [(positionOf sources (annotationProblemSpan p), D.This "this suppression")]
    []

-- | The rule id is the code, because it is also what a reader has to type to
-- suppress the finding. The line from the standards goes under the marker, so
-- that the answer to "says who" is next to the code.
--
-- A finding whose rule this set does not have still says what it found and
-- where: that is a report from another rule set being drawn by this one, and
-- only what the standards say about it is missing.
findingReport :: RuleSet -> SourceMap -> Finding -> D.Report String
findingReport rs sources f =
  let rid = T.unpack (ruleIdText (findingRule f))
   in case ruleFor rs (findingRule f) of
        Nothing ->
          D.Err
            (Just rid)
            (T.unpack (findingMessage f))
            [(positionOf sources (findingSpan f), D.This "reported here")]
            [D.Note (unwords ["This run has no rule called", rid ++ ", so what it asks for cannot be shown."])]
        Just rule ->
          D.Err
            (Just rid)
            (T.unpack (findingMessage f))
            [(positionOf sources (findingSpan f), D.This (T.unpack (ruleText rule)))]
            [ D.Note (T.unpack (ruleWhy rule)),
              D.Hint (suppressionHint sources f)
            ]

-- | Where to write the suppression, named as a place rather than gestured at.
--
-- A file-scoped suppression reaches the whole file and needs no line; a
-- site-scoped one has to attach to the code it is about. Which it is comes from
-- 'suppressionIsFileScoped' rather than a guess, so the place named and the
-- text offered cannot disagree.
--
-- The line named is not always the one the finding points at: a comment above
-- another comment attaches to that comment, so the line to name is the first at
-- or below the finding that is not itself a comment. Without the source there
-- is no line to name, which is what a project run from fact files alone has.
suppressionHint :: SourceMap -> Finding -> String
suppressionHint (SourceMap srcs) f =
  unwords
    [ "To suppress, write this",
      place ++ ", and say why:",
      T.unpack (suppressionFor f "<reason>")
    ]
  where
    place :: String
    place
      | suppressionIsFileScoped f = "anywhere in the file"
      | otherwise = case M.lookup (spanFile (findingSpan f)) srcs of
          Nothing -> "on the line above the code it points at"
          Just src ->
            unwords
              [ "directly above line",
                show (suppressionLineIn src (findingSpan f)),
                "of",
                T.unpack (relPathText (spanFile (findingSpan f)))
              ]

-- | The line a site-scoped suppression has to be written above for it to attach
-- to the code the finding is about.
suppressionLineIn :: Text -> Span -> Word
suppressionLineIn src sp = go (positionLine (spanStart sp))
  where
    lineMap :: M.Map Word Text
    lineMap = M.fromList (zip [(1 :: Word) ..] (T.lines src))

    lastLine :: Word
    lastLine = maybe 0 fst (M.lookupMax lineMap)

    go :: Word -> Word
    go n
      | n > lastLine = positionLine (spanStart sp)
      | otherwise = case M.lookup n lineMap of
          Just line | isCommentOnly line -> go (n + 1)
          _ -> n

    isCommentOnly :: Text -> Bool
    isCommentOnly line =
      let s = T.strip line
       in T.isPrefixOf "--" s || T.isPrefixOf "{-" s

unusedReport :: SourceMap -> AnnotationFact -> D.Report String
unusedReport sources a =
  D.Err
    (Just "UNUSED_SUPPRESSION")
    (concat ["[allow:", T.unpack (ruleIdText (annotationFactRule a)), "] suppresses nothing."])
    [(positionOf sources (annotationFactSpan a), D.This "nothing here is reported")]
    [D.Hint "Remove it. A suppression that has outlived its reason is worse than no suppression."]

overBroadReport :: SourceMap -> OverBroad -> D.Report String
overBroadReport sources ob =
  let a = overBroadAnnotation ob
      n = overBroadCount ob
   in D.Err
        (Just "OVER_BROAD_SUPPRESSION")
        ( unwords
            [ concat ["[allow:", T.unpack (ruleIdText (annotationFactRule a)), "]"],
              "suppresses",
              show n,
              "findings at once."
            ]
        )
        [(positionOf sources (annotationFactSpan a), D.This "this suppression")]
        [D.Hint "Place it on the statement instead, or fix one of them."]

-- | GHC's end column is one past the last character, which is exactly what
-- diagnose underlines up to.
--
-- A span that starts and ends in one place underlines nothing, which is what a
-- whole-file span is, so it is widened to cover the first line. Widened here
-- rather than where the span is made, because the line length is a property of
-- the source and only the renderer has it.
-- diagnose counts with 'Int', so this is where the one-based 'Word' of a
-- position becomes one again.
positionOf :: SourceMap -> Span -> D.Position
positionOf sources sp =
  D.Position
    { D.begin = (asInt (positionLine (spanStart sp)), asInt (positionCol (spanStart sp))),
      D.end =
        if spanStart sp == spanEnd sp
          then (asInt (positionLine (spanStart sp)), asInt (endOfLine sources (spanFile sp) (positionLine (spanStart sp))))
          else (asInt (positionLine (spanEnd sp)), asInt (positionCol (spanEnd sp))),
      D.file = T.unpack (relPathText (spanFile sp))
    }
  where
    asInt :: Word -> Int
    asInt = fromIntegral

-- | One past the last character of a line, or one past the start when the line
-- is not there to measure.
endOfLine :: SourceMap -> Path Rel File -> Word -> Word
endOfLine (SourceMap sources) rp line =
  case M.lookup rp sources of
    Nothing -> 2
    Just contents -> case drop (fromIntegral line - 1) (T.lines contents) of
      (l : _) -> fromIntegral (T.length l) + 1
      [] -> 2

-- | A report as an artifact: the data, and the rendering.
--
-- The rendering is written by whoever has the source rather than by whatever
-- judges the report later, because rendering needs the source.
--
-- Encoded here rather than written through a text handle, because a report is
-- drawn with box-drawing characters and a Nix build runs under the C locale,
-- where writing one through the locale encoding fails outright.
writeReportTo :: RuleSet -> Path Abs Dir -> SourceMap -> Complaints -> IO ()
writeReportTo rs dir sources report = do
  ensureDir dir
  LB.writeFile (toFilePath (dir </> reportDataFile)) (encodeReport report)
  BS.writeFile (toFilePath (dir </> reportTextFile)) (TE.encodeUtf8 (renderReport rs sources report))

-- | The source of every file the report points into, and a complaint for every
-- file the roots given do not account for.
--
-- One root per package, because the project layer runs on fact files whose
-- paths are repository-relative while the sources it is given are per-package
-- subtrees. A mapping that names nothing would otherwise be a report with
-- nothing under it and nobody the wiser.
sourcesForReport :: [SourceRoot] -> Complaints -> IO (SourceMap, Complaints)
sourcesForReport roots report = do
  found <- traverse readOne (reportFiles report)
  pure
    ( SourceMap (M.fromList (rights found)),
      complaintsOf (map ComplaintFailure (lefts found))
    )
  where
    -- Read rather than asked about and then read: the answer wanted is the
    -- text, and a file that is there for the question and gone for the read is
    -- a report with nothing under it.
    readOne rp = do
      read' <- traverse (forgivingAbsence . readSource) (mapMaybe (`sourceFileIn` rp) roots)
      pure $ case catMaybes read' of
        [] -> Left (NoSourceFor rp)
        (contents : _) -> Right (rp, contents)
