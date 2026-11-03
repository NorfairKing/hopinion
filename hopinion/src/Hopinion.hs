{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The commands, and the one place that decides what an exit code means.
--
-- There are two of those, and only two: a command either produces a report or
-- judges one. Producing succeeds whatever is in the report, so that the report
-- survives as an artifact to look at; judging is what fails.
module Hopinion (hopinionWith) where

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hopinion.Choices (ChoicesFileError, noChoices, readChoicesFrom, renderChoicesFileError, unChoices)
import Hopinion.OptParse
import Hopinion.Project (SourceRoot (..))
import Hopinion.Report
import Hopinion.Report.Render
import Hopinion.Rule (Rule, RuleSet, RuleSetError, renderRuleSetError, ruleSet, withoutRules)
import Hopinion.Rule.Id (RuleId)
import Hopinion.Run
import Path (Abs, Dir, File, Path, SomeBase (..), (</>))
import Path.IO (getCurrentDir)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hSetEncoding, stderr, stdout, utf8)
import Text.Colour (Chunk, chunk, fore, red)
import Text.Colour.Term (hPutChunksLocale, putChunksLocale)

-- | hopinion with a rule set of your own, which is the whole of how a
-- repository adds rules: an executable that depends on this library and on
-- @hopinion-rules@, passes @builtinRules ++ itsOwn@, and is what the check builders
-- are pointed at.
-- The rules hopinion ships are written against this library the same way, so
-- a rule never needs a change here.
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
        ids <- first StartupErrorChoicesUnreadable off
        first StartupErrorRulesUnusable (ruleSet rules [] >>= withoutRules ids)
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
  = StartupErrorChoicesUnreadable !ChoicesFileError
  | StartupErrorRulesUnusable !RuleSetError

renderStartupError :: StartupError -> [Chunk]
renderStartupError = \case
  StartupErrorChoicesUnreadable err -> [chunk (renderChoicesFileError err)]
  StartupErrorRulesUnusable err -> renderRuleSetError err

-- | Which rules this run is to leave alone, which is what the repository's file
-- says and nothing else. There is no flag for it, so the answer cannot differ
-- between the shell and the build.
--
-- Every command is handed a package or package outputs rather than a
-- repository, so each has to be told where the file is.
disabledFor :: Settings -> IO (Either ChoicesFileError [RuleId])
disabledFor settings = do
  told <- case settingsHopinionFile settings of
    Nothing -> pure (Right noChoices)
    Just named -> readChoicesFrom =<< absoluteFile named
  pure (unChoices <$> told)

run :: RuleSet -> Settings -> IO ()
run rs settings =
  case settingsDispatch settings of
    DispatchPackage root dir out hieDirs -> do
      absRoot <- absolutise root
      absDir <- absoluteDir dir
      absOut <- absoluteDir out
      absHie <- hieDirectoriesOf hieDirs
      report <- runPackageCommand rs absHie absRoot absDir absOut
      emit rs [absRoot] absOut report
    DispatchProject packageDirs expected sources out hieDirs -> do
      roots <- traverse absolutise sources
      absPackageDirs <- traverse absoluteDir packageDirs
      absOut <- absoluteDir out
      absHie <- hieDirectoriesOf hieDirs
      report <- runProjectCommand rs absHie absPackageDirs expected absOut
      emit rs roots absOut report
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
      ExplanationGiven cs -> putChunksLocale cs
      ExplanationNoRuleCalled cs -> do
        hPutChunksLocale stderr cs
        exitWith (ExitFailure 1)

-- | One code for "there is something to answer for" and one for "there is not".
-- Which kind of complaint it is belongs in the report, where it can be read,
-- rather than in a number that has to be looked up.
verdict :: Complaints -> ExitCode
verdict report = if isClean report then ExitSuccess else ExitFailure 1

-- | The artifact trees as typed, resolved against the working directory like
-- every other path a person can write on a command line.
hieDirectoriesOf :: NonEmpty (SomeBase Dir) -> IO HieDirectories
hieDirectoriesOf dirs = HieDirectories <$> traverse absoluteDir dirs

-- | Resolve a source root as typed against the working directory, which is the
-- one place that turns what a person wrote into a path the rest of the tool can
-- join and compare.
absolutise :: RawSourceRoot -> IO SourceRoot
absolutise raw = do
  dir <- absoluteDir (rawSourceRootDir raw)
  pure
    SourceRoot
      { sourceRootDir = dir,
        sourceRootPrefix = rawSourceRootPrefix raw
      }

absoluteDir :: SomeBase Dir -> IO (Path Abs Dir)
absoluteDir = \case
  Abs d -> pure d
  Rel d -> (</> d) <$> getCurrentDir

absoluteFile :: SomeBase File -> IO (Path Abs File)
absoluteFile = \case
  Abs f -> pure f
  Rel f -> (</> f) <$> getCurrentDir

-- | Say what was found and write it to the output directory.
--
-- Reading the sources can add complaints of its own, so those are said and
-- written too, where the judge will read them.
--
-- Findings go to stderr, as a compiler's do, so that a caller capturing the
-- tool's ordinary output gets none of the diagnostics.
emit :: RuleSet -> [SourceRoot] -> Path Abs Dir -> Complaints -> IO ()
emit rs roots out incoming = do
  (sources, missing) <- sourcesForReport roots incoming
  let report = incoming <> missing
  printReport rs stderr sources report
  writeReportTo rs out sources report
