{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The commands, and the one place that decides what an exit code means.
--
-- There are two of those, and only two: a command either produces a report or
-- judges one. Producing succeeds whatever is in the report, so that the report
-- survives as an artifact to look at; judging is what fails.
module Hopinion (hopinion, hopinionWith) where

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hopinion.Choices (ChoicesFileError, choicesDisabled, noChoices, readChoicesFrom, renderChoicesFileError)
import Hopinion.Comment
import Hopinion.Facts.Module
import Hopinion.Facts.Name
import Hopinion.Facts.Place
import Hopinion.OptParse
import Hopinion.Project (SourceRoot (..))
import Hopinion.Report
import Hopinion.Report.Render
import Hopinion.Rule (Rule, RuleSet, RuleSetError, renderRuleSetError, ruleSet, withoutRules)
import Hopinion.Rule.Id (RuleId)
import Hopinion.Rule.Registry (builtinRules)
import Hopinion.Run
import Path (Abs, Dir, File, Path, SomeBase (..), (</>))
import Path.IO (getCurrentDir, resolveDir')
import System.Exit (ExitCode (..), exitWith)
import System.IO (hSetEncoding, stderr, stdout, utf8)
import Text.Colour (Chunk, chunk, fore, red)
import Text.Colour.Term (hPutChunksLocale, putChunksLocale)

-- | hopinion with the rules it ships with.
hopinion :: IO ()
hopinion = hopinionWith builtinRules

-- | hopinion with a rule set of your own, which is the whole of how a
-- repository adds rules: an executable that depends on this library, passes
-- @builtinRules ++ itsOwn@, and is what the check builders are pointed at.
--
-- Turning a rule off is not this. That is `hopinion.yaml` at the repository
-- root, so a repository that only wants fewer rules needs no executable of its
-- own.
hopinionWith :: [Rule] -> IO ()
hopinionWith rules = do
  -- Reports are drawn with box-drawing characters, and a Nix build runs under
  -- the C locale, where the default handle encoding cannot write them at all:
  -- the first finding would kill the tool instead of being reported.
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  settings <- getSettings
  off <- disabledFor settings
  let started = do
        ids <- first ChoicesUnreadable off
        first RulesUnusable (ruleSet rules [] >>= withoutRules ids)
  case started of
    -- Nothing that stops a run this early is something to report about the
    -- code: it is a mistake in the executable, in the arguments, or in the
    -- repository's own file. So it goes to stderr and stops here, rather than
    -- into a report that would read as a finding.
    Left err -> do
      hPutChunksLocale stderr (fore red (chunk "hopinion: ") : renderStartupError err ++ [chunk "\n"])
      exitWith (ExitFailure 1)
    Right rs -> run rs settings

-- | What can stop a run before it has read any code.
data StartupError
  = ChoicesUnreadable !ChoicesFileError
  | RulesUnusable !RuleSetError

renderStartupError :: StartupError -> [Chunk]
renderStartupError = \case
  ChoicesUnreadable err -> [chunk (renderChoicesFileError err)]
  RulesUnusable err -> renderRuleSetError err

-- | Which rules this run is to leave alone, which is what the repository's file
-- says and nothing else. There is no flag for it, so the answer cannot differ
-- between the shell and the build.
--
-- Only for the commands that are handed no repository and so have to be told
-- where the file is; `check` is handed one and finds it for itself in
-- 'runCheck'.
disabledFor :: Settings -> IO (Either ChoicesFileError [RuleId])
disabledFor settings = do
  told <- case settingHopinionFile settings of
    Nothing -> pure (Right noChoices)
    Just named -> readChoicesFrom =<< absoluteFile named
  pure (choicesDisabled <$> told)

run :: RuleSet -> Settings -> IO ()
run rs settings =
  case settingDispatch settings of
    DispatchCheck root hieDirs -> do
      absRoot <- absolutise root
      absHie <- hieDirectoriesOf hieDirs
      report <- emit rs [absRoot] Nothing =<< runCheck rs absHie absRoot
      -- The development loop is a decider, because a person running it wants
      -- the shell to know the answer.
      exitWith (verdict report)
    DispatchPackage root dir out hieDirs -> do
      absRoot <- absolutise root
      absDir <- absoluteDir dir
      absOut <- traverse absoluteDir out
      absHie <- hieDirectoriesOf hieDirs
      report <- runPackageCommand rs absHie absRoot absDir absOut
      _ <- emit rs [absRoot] absOut report
      pure ()
    DispatchProject packageDirs expected sources out hieDirs -> do
      roots <- traverse absolutise sources
      absPackageDirs <- traverse absoluteDir packageDirs
      absOut <- traverse absoluteDir out
      absHie <- hieDirectoriesOf hieDirs
      report <- runProjectCommand rs absHie absPackageDirs expected absOut
      _ <- emit rs roots absOut report
      pure ()
    DispatchModule relFile extensions component dump -> do
      facts <- factsForFile rs relFile extensions component
      case dump of
        NoDumpComments -> pure ()
        DumpComments -> mapM_ (TIO.putStrLn . renderComment) (moduleContextComments facts)
      -- The module command reports paths exactly as it was given them, so the
      -- working directory is what they are relative to.
      root <- resolveDir' "."
      report <-
        emit
          rs
          [SourceRoot {sourceRootDir = root, sourceRootPrefix = Nothing}]
          Nothing
          (runModuleLayer rs facts)
      exitWith (verdict report)
    DispatchJudge dirs -> do
      results <- traverse (absoluteDir >=> readReportFrom) dirs
      case sequence results of
        Left err -> do
          TIO.hPutStrLn stderr (T.concat ["hopinion: ", renderReportDirError err])
          exitWith (ExitFailure 1)
        Right reports -> do
          mapM_ (TIO.hPutStr stderr . snd) reports
          exitWith (verdict (foldMap fst reports))
    DispatchListRules -> putChunksLocale (listRules rs)
    DispatchExplain rid -> case explainRule rs rid of
      Explained cs -> putChunksLocale cs
      NoRuleCalled cs -> do
        hPutChunksLocale stderr cs
        exitWith (ExitFailure 1)

-- | One code for "there is something to answer for" and one for "there is not".
-- Which kind of complaint it is belongs in the report, where it can be read,
-- rather than in a number that has to be looked up.
verdict :: Complaints -> ExitCode
verdict report = if isClean report then ExitSuccess else ExitFailure 1

-- | The artifact trees as typed, resolved against the working directory like
-- every other path a person can write on a command line.
hieDirectoriesOf :: [SomeBase Dir] -> IO HieDirectories
hieDirectoriesOf dirs = HieDirectories <$> traverse absoluteDir dirs

-- | Resolve a source root as typed against the working directory, which is the
-- one place that turns what a person wrote into a path the rest of the tool can
-- join and compare.
absolutise :: RawSourceRoot -> IO SourceRoot
absolutise raw = do
  dir <- absoluteDir (rawSourceRootDir raw)
  pure SourceRoot {sourceRootDir = dir, sourceRootPrefix = rawSourceRootPrefix raw}

absoluteDir :: SomeBase Dir -> IO (Path Abs Dir)
absoluteDir = \case
  Abs d -> pure d
  Rel d -> (</> d) <$> getCurrentDir

absoluteFile :: SomeBase File -> IO (Path Abs File)
absoluteFile = \case
  Abs f -> pure f
  Rel f -> (</> f) <$> getCurrentDir

-- | Say what was found, write it where it was asked for, and hand back what was
-- said, which is what to judge: reading the sources can add complaints of its
-- own, and judging what came in would miss them.
--
-- Findings go to stderr, as a compiler's do, so that a caller capturing the
-- tool's ordinary output gets none of the diagnostics.
emit :: RuleSet -> [SourceRoot] -> Maybe (Path Abs Dir) -> Complaints -> IO Complaints
emit rs roots reportOut incoming = do
  (sources, missing) <- sourcesForReport roots incoming
  let report = incoming <> missing
  printReport rs stderr sources report
  case reportOut of
    Nothing -> pure ()
    Just dir -> writeReportTo rs dir sources report
  pure report

renderComment :: CommentFact -> T.Text
renderComment cf =
  T.concat
    [ relPathText (spanFile (commentFactSpan cf)),
      ":",
      T.pack (show (positionLine (spanStart (commentFactSpan cf)))),
      ":",
      T.pack (show (positionCol (spanStart (commentFactSpan cf)))),
      ": ",
      T.pack (show (commentFactStyle cf)),
      " ",
      T.pack (show (commentFactAttachment cf)),
      " ",
      T.pack (show (commentFactText cf))
    ]
