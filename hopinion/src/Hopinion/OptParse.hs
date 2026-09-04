{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Hopinion.OptParse
  ( Settings (..),
    Dispatch (..),
    DumpComments (..),
    RawSourceRoot (..),
    getSettings,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import Hopinion.Facts.Component (ComponentKind (..), componentKindText, parseComponentKind)
import Hopinion.Rule.Id (RuleId, parseRuleId)
import OptEnvConf
import Path (Dir, File, Path, Rel, SomeBase (..), parseRelDir, parseRelFile, parseSomeDir, parseSomeFile, reldir)
import Paths_hopinion (version)

data Settings = Settings
  { settingDispatch :: !Dispatch,
    -- | Where the repository says which rules it has decided against, when the
    -- command was given a package rather than a repository and so cannot look
    -- beside it.
    settingHopinionFile :: !(Maybe (SomeBase File))
  }

-- | A source root as it was typed, before anything has looked at the disk.
--
-- 'SomeBase' rather than an absolute directory, because which of the two it is
-- is all a parser can know: making it absolute needs the working directory,
-- which is IO, and the command layer does that. What a parser can refuse is
-- text that is no directory at all, and it does.
--
-- The prefix is relative by construction: it is what a reported path is
-- prefixed with, and an absolute one would name a place outside the repository.
data RawSourceRoot = RawSourceRoot
  { rawSourceRootDir :: !(SomeBase Dir),
    rawSourceRootPrefix :: !(Maybe (Path Rel Dir))
  }
  deriving (Show, Eq)

data Dispatch
  = DispatchCheck !RawSourceRoot ![SomeBase Dir]
  | DispatchPackage !RawSourceRoot !(SomeBase Dir) !(Maybe (SomeBase Dir)) ![SomeBase Dir]
  | DispatchProject ![SomeBase Dir] ![Text] ![RawSourceRoot] !(Maybe (SomeBase Dir)) ![SomeBase Dir]
  | DispatchModule !(Path Rel File) ![Text] !ComponentKind !DumpComments
  | DispatchJudge !(NonEmpty (SomeBase Dir))
  | DispatchListRules
  | DispatchExplain !RuleId

-- | Whether the module command prints every comment with what it attached to,
-- which is for debugging the attachment pass rather than for checking anything.
data DumpComments
  = NoDumpComments
  | DumpComments
  deriving stock (Show, Eq)

getSettings :: IO Settings
getSettings = runSettingsParser version "hopinion: enforce the code review standards"

instance HasParser Settings where
  settingsParser = parseSettings

parseSettings :: Parser Settings
parseSettings = withoutConfig $ do
  settingDispatch <- parseDispatch
  -- Where the file is, never what is in it: nothing about a run is configured on
  -- a command line.
  --
  -- For the two commands that cannot find it. A package derivation is handed one
  -- package's subtree, and the project command is handed fact files and no
  -- source at all; @check@ is handed the repository and looks beside it.
  settingHopinionFile <-
    optional
      ( setting
          [ help "The repository's hopinion.yaml, for a command that was given a package rather than a repository",
            reader (maybeReader parseSomeFile),
            option,
            long "hopinion-file",
            metavar "FILE"
          ]
      )
  pure Settings {..}

parseDispatch :: Parser Dispatch
parseDispatch =
  commands
    [ command "check" "Check a whole project in one process" (parseRepository DispatchCheck),
      command "module" "Check one module, for an editor or for debugging" parseModule,
      command "package" "Check one package and write its facts" parsePackage,
      command "project" "Check a project from package facts alone" parseProject,
      command "judge" "Fail if any of these reports has something to answer for" parseJudge,
      command "list-rules" "Print every rule with its level and class" (pure DispatchListRules),
      command "explain" "Print a rule's text and where it comes from" parseExplain
    ]

-- | Both commands take a repository and the artifacts of building it, if there
-- are any. The prefix is empty because a repository is what reported paths are
-- already relative to; only a build that sees one package's subtree in
-- isolation needs one.
parseRepository :: (RawSourceRoot -> [SomeBase Dir] -> Dispatch) -> Parser Dispatch
parseRepository f = do
  hieDirs <- parseHieDirectories
  dir <-
    setting
      [ help "The repository root",
        reader (maybeReader parseSomeDir),
        argument,
        metavar "DIR",
        value (Rel [reldir|.|])
      ]
  pure (f RawSourceRoot {rawSourceRootDir = dir, rawSourceRootPrefix = Nothing} hieDirs)

parsePackage :: Parser Dispatch
parsePackage = do
  out <- parseReportOut
  hieDirs <- parseHieDirectories
  root <- parseSourceRoot
  dir <-
    setting
      [ help "The package directory",
        reader (maybeReader parseSomeDir),
        argument,
        metavar "DIR"
      ]
  pure (DispatchPackage root dir out hieDirs)

-- | Where a build put its @.hie@ output, so that a rule can ask what a splice
-- generated instead of guessing.
--
-- Passing none is allowed, which is what the module command and a run over a
-- tree nobody compiled do. Passing any is a promise that they cover every
-- module read, and the project phase holds the run to it, because a module a
-- build left out reads exactly like a module there is nothing to say about.
parseHieDirectories :: Parser [SomeBase Dir]
parseHieDirectories =
  many
    ( setting
        [ help "A directory of .hie files, as a build with -fwrite-ide-info leaves behind. Given any, they must cover every module",
          reader (maybeReader parseSomeDir),
          option,
          long "hie-directory",
          metavar "DIR"
        ]
    )

-- | Producing commands succeed whatever they find, so what they produced has to
-- go somewhere a later command can read and a person can look at: the facts, the
-- report as data, and the report as text.
parseReportOut :: Parser (Maybe (SomeBase Dir))
parseReportOut =
  optional
    ( setting
        [ help "A directory to write this run's facts and report to",
          reader (maybeReader parseSomeDir),
          option,
          long "out",
          metavar "DIR"
        ]
    )

-- | At least one report, because judging nothing is the one way this command
-- can answer "clean" without having read anything. A caller that passes none
-- has wired something up wrong, and the reports it meant to hand over would
-- never be looked at.
parseJudge :: Parser Dispatch
parseJudge = do
  first <-
    setting
      [ help "A report directory, as written by --report-out",
        reader (maybeReader parseSomeDir),
        argument,
        metavar "DIR"
      ]
  rest <-
    many
      ( setting
          [ help "Another report directory",
            reader (maybeReader parseSomeDir),
            argument,
            metavar "DIR"
          ]
      )
  pure (DispatchJudge (first :| rest))

parseProject :: Parser Dispatch
parseProject = do
  -- Read before the rest so that this block ends in a bind that is not also
  -- the last argument, which is the shape hlint asks to be written with <$>
  -- and ApplicativeDo cannot express.
  reportOut <- parseReportOut
  hieDirs <- parseHieDirectories
  packageDirs <-
    many
      ( setting
          [ help "A package's output directory, as written by the package command",
            reader (maybeReader parseSomeDir),
            option,
            long "package",
            metavar "DIR"
          ]
      )
  expected <-
    setting
      [ help "The packages that must be present, comma separated. A missing one is an error, never an empty set",
        reader str,
        option,
        long "expect-packages",
        metavar "NAMES"
      ]
  -- Facts carry repository-relative paths and the sources under Nix are
  -- per-package subtrees, so showing a finding against its code needs both
  -- halves of that mapping. Optional: without it the findings still print,
  -- with the position but no snippet.
  sources <-
    many
      ( setting
          [ help "Where a package's sources are, as PREFIX=DIR, so that findings can be shown against the code",
            reader (maybeReader sourceRootOf),
            option,
            long "source",
            metavar "PREFIX=DIR"
          ]
      )
  pure (DispatchProject packageDirs (T.splitOn "," (T.pack expected)) sources reportOut hieDirs)

-- | A half that is not a path is refused here, rather than carried as text to
-- wherever would otherwise have to decide what it meant.
sourceRootOf :: String -> Maybe RawSourceRoot
sourceRootOf s = case break (== '=') s of
  (_, "") -> Nothing
  (prefix, _ : dir) -> do
    parsedDir <- parseSomeDir dir
    parsedPrefix <- parseRelDir prefix
    Just
      RawSourceRoot
        { rawSourceRootDir = parsedDir,
          rawSourceRootPrefix = Just parsedPrefix
        }

parseModule :: Parser Dispatch
parseModule = do
  file <-
    setting
      [ help "The module to check, as a path relative to the directory hopinion runs in, because that is what its findings are reported against",
        reader (maybeReader parseRelFile),
        argument,
        metavar "FILE"
      ]
  extensions <-
    many
      ( setting
          [ help "An extension the module's cabal component enables by default",
            reader str,
            option,
            long "extension",
            metavar "EXTENSION"
          ]
      )
  component <-
    setting
      [ help "Which sort of component the module belongs to",
        reader (maybeReader (parseComponentKind . T.pack)),
        option,
        long "component",
        metavar (T.unpack (T.intercalate "|" (map componentKindText [minBound .. maxBound]))),
        value ComponentLib
      ]
  dump <-
    setting
      [ help "Print every comment with its attachment",
        switch DumpComments,
        long "dump-comments",
        value NoDumpComments
      ]
  pure (DispatchModule file (map T.pack extensions) component dump)

parseSourceRoot :: Parser RawSourceRoot
parseSourceRoot = do
  dir <-
    setting
      [ help "The directory reported paths are computed against",
        reader (maybeReader parseSomeDir),
        option,
        long "root",
        metavar "DIR",
        value (Rel [reldir|.|])
      ]
  prefix <-
    optional
      ( setting
          [ help "Prepended to every reported path, for a build that sees one package's subtree in isolation",
            reader (maybeReader parseRelDir),
            option,
            long "rel-prefix",
            metavar "DIR"
          ]
      )
  pure RawSourceRoot {rawSourceRootDir = dir, rawSourceRootPrefix = prefix}

parseExplain :: Parser Dispatch
parseExplain = do
  rid <-
    setting
      [ help "The rule to explain",
        reader (maybeReader (parseRuleId . T.pack)),
        argument,
        metavar "RULE-ID"
      ]
  pure (DispatchExplain rid)
