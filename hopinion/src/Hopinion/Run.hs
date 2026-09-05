{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The commands, over everything else.
--
-- @check@ and the split package-plus-project path share their layers but not
-- their plumbing: @check@ round-trips facts through the codec in memory, so the
-- property that the two agree can still catch a fact a project rule needs and
-- extraction does not serialise.
--
-- Every command takes the rule set it runs rather than reading one this module
-- knows, which is what lets a repository add rules and turn others off.
module Hopinion.Run
  ( runCheck,
    HieDirectories (..),
    noHieDirectories,
    runPackageCommand,
    runProjectCommand,
    runModuleCommand,
    factsForFile,
    factsForSource,
    runModuleLayer,
    listRules,
    Explanation (..),
    explainRule,
    storeMigrations,
    writePackage,
    runPackagePhase,
    runProjectPhase,
    runCheckWithoutChoices,
  )
where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.List (nub, sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sql (Migration)
import Hopinion.Annotation
import Hopinion.Choices (choicesDisabled, readChoicesIn, renderChoicesFileError)
import Hopinion.Compiled
import Hopinion.Extract
import Hopinion.Facts.Component
import Hopinion.Facts.Module
import Hopinion.Facts.Name
import Hopinion.Facts.Outcome
import Hopinion.Facts.Place
import Hopinion.Facts.Suppression
import Hopinion.Facts.Version
import Hopinion.Parse
import Hopinion.Project
import Hopinion.Report
import Hopinion.Rule
import Hopinion.Rule.Id
import Hopinion.Store
import Path (Abs, Dir, File, Path, Rel, toFilePath, (</>))
import Path.IO (ensureDir, resolveFile')
import Text.Colour (Chunk, TerminalCapabilities (..), blue, chunk, fore, red, renderChunksText)
import UnliftIO.Async (pooledMapConcurrently)

-- | Where a run's store lives: in the output directory when there is one, and
-- in memory when there is not.
storeAt :: Maybe (Path Abs Dir) -> StoreLocation
storeAt = maybe InMemory (OnDisk . (</> factsFile))

-- | Every table the envelope and the rules between them need.
--
-- Every rule this build has, not only the ones this run makes: a store crosses a
-- process boundary, so a table the package runs filled and the project run
-- never created is a merge with nowhere to put a row. An empty table costs a
-- row in the schema.
storeMigrations :: RuleSet -> [Migration]
storeMigrations rs = ruleMigrations (ruleSetRules rs ++ ruleSetTurnedOff rs)

-- | The whole project in one process, which is the development loop.
--
-- One store with nowhere to put it, so the phases run over exactly the code the
-- layered path runs over and the two can be asserted to agree.
--
-- Narrows the given set by what the repository decided, this being the command
-- handed a repository and so the one that can find its file. A file that is
-- there and wrong is a complaint in the report like any other.
runCheck :: RuleSet -> HieDirectories -> SourceRoot -> IO Complaints
runCheck given hieDirs root = do
  decided <- readChoicesIn (sourceRootDir root)
  case decided of
    Left err -> pure (failureComplaints [ChoicesRefused (renderChoicesFileError err)])
    Right choices -> case withoutRules (choicesDisabled choices) given of
      Left err -> pure (failureComplaints [RuleSetRefused (renderChunksText WithoutColours (renderRuleSetError err))])
      Right rs -> runCheckWithoutChoices rs hieDirs root

-- | The same, over the rule set exactly as given.
--
-- Exported so a test can ask what a run would have said without the file, which
-- is what makes a test of the file about the file.
runCheckWithoutChoices :: RuleSet -> HieDirectories -> SourceRoot -> IO Complaints
runCheckWithoutChoices rs hieDirs root =
  reportingArtifactProblems $
    withPackages root $ \models ->
      withMemoryStore (storeMigrations rs) $ do
        writeMeta
        written <- traverse (writePackage rs hieDirs root) models
        -- One value per module, handed to each phase at its own scope, so a
        -- module is read once for all three and never if nothing asks.
        let byPackage = M.fromList (zip (map packageModelName models) (map snd written))
        names <- packageNames
        packageReports <- traverse (\n -> runPackagePhase rs n (M.findWithDefault mempty n byPackage)) names
        projectReport <- projectReportFor rs names (mconcat (map snd written))
        pure (mconcat (map fst written) <> mconcat packageReports <> projectReport)

runPackageCommand :: RuleSet -> HieDirectories -> SourceRoot -> Path Abs Dir -> Maybe (Path Abs Dir) -> IO Complaints
runPackageCommand rs hieDirs root dir mOut = reportingArtifactProblems $ do
  eModels <- discoverPackages root dir
  case eModels of
    Left err -> pure (failureComplaints [RepositoryUnreadable (renderDiscoveryError err)])
    Right [] -> pure (failureComplaints [RepositoryUnreadable (renderDiscoveryError (NoCabalFileUnder dir))])
    Right (pm : _) -> do
      traverse_ ensureDir mOut
      withStore (storeMigrations rs) (storeAt mOut) $ do
        writeMeta
        (moduleReport, compiled) <- writePackage rs hieDirs root pm
        packageReport <- runPackagePhase rs (packageModelName pm) compiled
        pure (moduleReport <> packageReport)

-- | The project phase takes what the package phase produced, both halves of it:
-- the store to make more findings out of, and the findings already made, which
-- it unions in rather than deriving again.
runProjectCommand :: RuleSet -> HieDirectories -> [Path Abs Dir] -> [Text] -> Maybe (Path Abs Dir) -> IO Complaints
runProjectCommand rs hieDirs packageDirs expectedNames mOut = reportingArtifactProblems $ do
  earlier <- traverse readReportFrom packageDirs
  -- Before the merge, which copies column by column and would fail on a name
  -- rather than on the version that explains it.
  foreignFormats <- unreadableFormats packageDirs
  case (sequence earlier, repeatedDirs packageDirs ++ foreignFormats) of
    (_, refused@(_ : _)) -> pure (failureComplaints (map PackageOutputRefused refused))
    (Left err, _) -> pure (failureComplaints [PackageOutputRefused (renderReportDirError err)])
    (Right reports, []) -> do
      traverse_ ensureDir mOut
      withStore (storeMigrations rs) (storeAt mOut) $ do
        writeMeta
        unmergeable <- concat <$> traverse (mergeStore . (</> factsFile)) packageDirs
        if not (null unmergeable)
          then pure (failureComplaints (map PackageOutputRefused unmergeable))
          else do
            -- This process never saw the source, so the modules come from the
            -- merged facts, which is also what keeps both paths asking about
            -- one set.
            modules <- storedModules
            compiled <-
              liftIO
                ( compiledModulesFor
                    hieDirs
                    [ (moduleRefModule (storedModuleModuleRef m), storedPathFile (storedModulePath m))
                    | m <- modules,
                      ParsedOk <- [storedModuleOutcome m]
                    ]
                )
            projectReport <- projectReportFor rs (map PackageName expectedNames) compiled
            pure (mconcat (map fst reports) <> projectReport)

-- | A build that could not answer for a module it was held to covering is
-- something the tool could not tell, so it belongs in the report. Let out as an
-- exception it would kill the run before a report had been written, and only
-- judging one is meant to fail.
reportingArtifactProblems :: IO Complaints -> IO Complaints
reportingArtifactProblems act = do
  result <- try act
  pure $ case result of
    Left (ArtifactProblem t) -> failureComplaints [ArtifactsRefused t]
    Right report -> report

-- | Every package output whose facts this build of the tool cannot read.
unreadableFormats :: [Path Abs Dir] -> IO [Text]
unreadableFormats dirs = do
  stamps <- traverse (\d -> (,) d <$> storedFormatOf (d </> factsFile)) (nub dirs)
  pure
    [ T.pack
        ( unwords
            [ "The facts in",
              toFilePath dir,
              "were written in format",
              concat [maybe "no format at all" show stamp, ","],
              "and this build of hopinion reads format",
              concat [show (formatVersionNumber currentFormatVersion), "."],
              "Build the package outputs and the project with one hopinion."
            ]
        )
    | (dir, stamp) <- stamps,
      stamp /= Just (formatVersionNumber currentFormatVersion)
    ]

-- | Every package directory given more than once.
--
-- Repeated input is an error for the reason absent input is. A rule's facts are
-- rows with nothing to collide on, so a directory passed twice is every one of
-- its findings reported twice.
repeatedDirs :: [Path Abs Dir] -> [Text]
repeatedDirs dirs =
  [ T.pack
      ( unwords
          [ "The package output",
            toFilePath dir,
            "was given more than once, and its facts are rows rather than keyed entries,",
            "so every finding in it would be reported once per copy."
          ]
      )
  | dir <- nub dirs,
    length (filter (== dir) dirs) > 1
  ]

projectReportFor :: RuleSet -> [PackageName] -> CompiledModules -> Query Complaints
projectReportFor rs expected compiled = case NE.nonEmpty (nub (sort expected)) of
  Nothing -> pure (failureComplaints [NoPackagesExpected])
  Just neExpected -> runProjectPhase rs neExpected compiled

-- | One module, for an editor or for debugging the parser. The context comes
-- from flags rather than from a cabal file, so this command never needs to
-- discover anything.
runModuleCommand :: RuleSet -> Path Rel File -> [Text] -> ComponentKind -> IO Complaints
runModuleCommand rs file extensions component =
  runModuleLayer rs <$> factsForFile rs file extensions component

-- | One file's facts, with the file standing in for the module name. Used by
-- the module command and by the tests, so neither reaches around the extraction
-- path a real run uses.
factsForFile :: RuleSet -> Path Rel File -> [Text] -> ComponentKind -> IO ModuleContext
factsForFile rs file extensions component = do
  here <- resolveFile' (toFilePath file)
  src <- readSource here
  factsForSource rs file extensions component src

-- | The same, over source that is not on disk, which is what asking "would this
-- reformatted module attach its comments the same way" needs.
factsForSource :: RuleSet -> Path Rel File -> [Text] -> ComponentKind -> Text -> IO ModuleContext
factsForSource rs rp extensions component src = do
  let input =
        ParseInput
          { parseInputRelPath = rp,
            parseInputDefaultExtensions = extensions,
            parseInputSource = src
          }
  let extractInput =
        ExtractInput
          { extractInputModule = ModuleKey (relPathText rp),
            extractInputRelPath = rp,
            extractInputComponent = component,
            -- The module command is given a file rather than a package, so
            -- there is no cabal file to say which component claims it.
            extractInputComponentName = ComponentName "lib",
            extractInputRules = rs
          }
  parseHaskellModule input >>= \case
    Left (errLoc, msg) -> pure (failedModuleContext extractInput errLoc msg)
    Right parsed -> pure (extractModuleContext extractInput parsed)

-- | Every rule that will run, and the ones turned off said as such: "quiet"
-- and "not asked" are what a reader of this list is telling apart.
listRules :: RuleSet -> [Chunk]
listRules rs =
  concat
    ( map (line False) (ruleSetRules rs)
        ++ map (line True) (ruleSetTurnedOff rs)
    )
  where
    line off r =
      [ fore blue (chunk (ruleIdText (ruleId r))),
        chunk "\t",
        if off then fore red (chunk "off") else chunk "runs",
        chunk "\t",
        chunk (levelText (ruleLevel r)),
        chunk "\n"
      ]

-- | What there is to say about a name somebody asked to have explained.
--
-- Two constructors rather than one 'Text', because the two end in different
-- places. An explanation is what the command was asked for. A name nothing
-- answers to is a question this build cannot answer, and answering it with
-- prose on the same stream and the same exit code leaves a caller unable to
-- tell the two apart.
data Explanation
  = Explained ![Chunk]
  | NoRuleCalled ![Chunk]
  deriving (Show, Eq)

-- | What a rule asks for and why, whether or not this run makes it.
--
-- A rule this repository has turned off can still be explained, and is said to
-- be off rather than said not to exist: it is in this build, so calling it
-- missing would send a reader looking for a typo they did not make.
explainRule :: RuleSet -> RuleId -> Explanation
explainRule rs rid = case ruleNamed rs rid of
  Nothing ->
    NoRuleCalled
      [ chunk "There is no rule called ",
        fore red (chunk (ruleIdText rid)),
        chunk " in this build of hopinion.\n",
        chunk "Run list-rules to see the ones there are.\n"
      ]
  Just r ->
    Explained
      ( concat
          [ [fore blue (chunk (ruleIdText (ruleId r))), chunk "\n"],
            [chunk (ruleText r), chunk "\n"],
            [chunk (ruleWhy r), chunk "\n"],
            [ chunk (T.concat ["Level: ", levelText (ruleLevel r)]),
              chunk "\n"
            ],
            turnedOff
          ]
      )
    where
      turnedOff :: [Chunk]
      turnedOff =
        [ fore red (chunk "This repository has turned it off in hopinion.yaml, so this run does not make it.\n")
        | useOf rs rid == Just RuleTurnedOff
        ]

withPackages :: SourceRoot -> ([PackageModel] -> IO Complaints) -> IO Complaints
withPackages root act = do
  eModels <- discoverPackages root (sourceRootDir root)
  case eModels of
    Left err -> pure (failureComplaints [RepositoryUnreadable (renderDiscoveryError err)])
    Right models -> act models

-- | One package, read once: the module phase over every module in it, writing
-- what the later phases need and returning what the module rules found.
--
-- What a build said comes back with it, since the package phase is about to
-- want all of it.
writePackage :: RuleSet -> HieDirectories -> SourceRoot -> PackageModel -> Query (Complaints, CompiledModules)
writePackage rs hieDirs root pm = do
  contexts <- liftIO (readModules rs (packageModelComponents pm))
  -- One deferred read per module, shared by all three scopes.
  compiled <-
    liftIO
      ( compiledModulesFor
          hieDirs
          [ (moduleContextModule ctx, moduleContextPath ctx)
          | ctx <- contexts,
            ParsedOk <- [moduleContextOutcome ctx]
          ]
      )
  unread <- liftIO (unreadSuppressionsOf rs root pm)
  writePackageEnvelope
    pkg
    (packageModelRole pm)
    (packageModelCabal pm)
    [ ModuleRef {moduleRefComponent = componentModelName c, moduleRefModule = m}
    | c <- packageModelComponents pm,
      m <- componentModelDeclaredModules c
    ]
  mapM_ writeContext contexts
  pure
    ( mconcat (map (runModuleLayer rs) contexts) <> unread,
      compiled
    )
  where
    pkg = packageModelName pm

    writeContext ctx = do
      writeModuleEnvelope
        pkg
        (moduleContextRef ctx)
        (moduleContextPath ctx)
        (moduleContextOutcome ctx)
        (moduleContextAnnotations ctx)
      mapM_ (\r -> carryOf r pkg ctx) (ruleSetRules rs)

-- | Every module of every component, read in parallel.
--
-- Reading is the whole cost of a run and modules do not depend on each other,
-- so this is the one place worth spending cores on. The store is written
-- afterwards, in one thread, in declaration order.
--
-- A pool rather than a thread per module: one parse per module in flight would
-- make a run's residency the repository's. @unliftio@ sizes the pool by the
-- capabilities the runtime was given, which is what @-N8@ in the executable's
-- options is choosing.
readModules :: RuleSet -> [ComponentModel] -> IO [ModuleContext]
readModules rs components =
  pooledMapConcurrently
    (uncurry (moduleContextFor rs))
    [(cm, mm) | cm <- components, mm <- componentModelModules cm]

moduleContextFor :: RuleSet -> ComponentModel -> ModuleModel -> IO ModuleContext
moduleContextFor rs cm mm = do
  let extractInput =
        ExtractInput
          { extractInputModule = moduleModelKey mm,
            extractInputRelPath = moduleModelRelPath mm,
            extractInputComponent = componentModelKind cm,
            extractInputComponentName = componentModelName cm,
            extractInputRules = rs
          }
  case moduleModelSource mm of
    PreprocessedSource -> pure (preprocessedModuleContext extractInput)
    HaskellSource -> do
      src <- readSource (moduleModelFile mm)
      let input =
            ParseInput
              { parseInputRelPath = moduleModelRelPath mm,
                parseInputDefaultExtensions = componentModelDefaultExtensions cm,
                parseInputSource = src
              }
      parseHaskellModule input >>= \case
        Left (errLoc, msg) -> pure (failedModuleContext extractInput errLoc msg)
        Right parsed -> pure (extractModuleContext extractInput parsed)

-- | One level's verdict: what its rules found, with its own suppressions given
-- the chance to answer for it.
--
-- The three layers differ only in what they run over and where their
-- suppressions live, so this is built once here.
layerComplaints :: RuleSet -> Level -> CheckResult -> [AnnotationFact] -> Complaints
layerComplaints rs level result inScope =
  let sup = applySuppression (annotationsAtLevel rs level inScope) (checkResultFindings result)
   in complaintsOf
        ( concat
            [ map ComplaintFinding (suppressionRemaining sup),
              map ComplaintUnused (suppressionUnused sup),
              map ComplaintOverBroad (suppressionOverBroad sup)
            ]
        )

-- | The module phase for one module, over what the parser saw.
--
-- A module that did not parse is a failure of the run rather than a module with
-- nothing wrong in it. One whose source is a preprocessor's input holds no
-- Haskell, so no rule runs over it and there is nothing to say about it.
runModuleLayer :: RuleSet -> ModuleContext -> Complaints
runModuleLayer rs mf =
  layerComplaints rs LevelModule (resultFor (moduleContextOutcome mf)) (moduleContextAnnotations mf)
    <> complaintsOf
      ( map ComplaintProblem (moduleContextAnnotationProblems mf)
          ++ map ComplaintFailure (failures (moduleContextOutcome mf))
      )
  where
    resultFor = \case
      ParsedOk -> foldMap (\c -> c mf) (moduleChecks rs)
      ParseFailed _ _ -> noResult
      NotHaskellSource -> noResult

    failures = \case
      ParsedOk -> []
      NotHaskellSource -> []
      ParseFailed pos msg ->
        [ ModuleDidNotParse
            ParseFailure
              { parseFailurePath = moduleContextPath mf,
                parseFailurePosition = pos,
                parseFailureMessage = msg
              }
        ]

-- | The package phase: every package rule, over what the modules wrote for it,
-- and over what a build said about this package's modules.
runPackagePhase :: RuleSet -> PackageName -> CompiledModules -> Query Complaints
runPackagePhase rs pkg compiled = do
  results <- traverse perRule (rulesAtLevel rs LevelPackage)
  annotations <- annotationsOfPackage pkg
  pure (layerComplaints rs LevelPackage (mconcat results) annotations)
  where
    perRule r = case ruleImpl r of
      PackageRule c -> packageCheckFindings c pkg compiled
      ModuleRule _ -> pure noResult
      ProjectRule _ -> pure noResult

-- | The project phase: every project rule, over what every module in every
-- package wrote for it, and over what a build said about all of them.
runProjectPhase :: RuleSet -> NonEmpty PackageName -> CompiledModules -> Query Complaints
runProjectPhase rs expected compiled = do
  uncovered <- liftIO (uncoveredModules compiled)
  problems <- storeProblems rs expected uncovered
  -- Every problem, not the first: a build that missed a dozen modules names a
  -- dozen, which at a rebuild apiece is one fix rather than twelve.
  if not (null problems)
    then pure (failureComplaints (map FactsIncomplete problems))
    else do
      results <- traverse perRule (rulesAtLevel rs LevelProject)
      layerComplaints rs LevelProject (mconcat results) <$> storedAnnotations
  where
    perRule r = case ruleImpl r of
      ProjectRule c -> projectCheckFindings c compiled
      ModuleRule _ -> pure noResult
      PackageRule _ -> pure noResult

-- | Absent input is an error, never an empty set. Every failure mode here would
-- otherwise turn into a silently satisfied obligation: a package whose facts
-- never arrived, a module the cabal file declares and nothing could read, or a
-- module a build was supposed to have compiled and did not.
storeProblems :: RuleSet -> NonEmpty PackageName -> [Path Rel File] -> Query [StoreProblem]
storeProblems rs expected uncovered = do
  present <- packageNames
  otherVersion <- writtenByAnotherVersion
  modules <- storedModules
  annotations <- storedAnnotations
  declared <- traverse (\p -> (,) p <$> expectedModulesOf p) present
  -- By component, not by module name: every component with a @main-is@
  -- declares a module called Main, and covering one of them is not covering
  -- the others.
  let covered =
        M.fromListWith
          S.union
          [(storedModulePackage m, S.singleton (storedModuleModuleRef m)) | m <- modules]
  pure
    ( concat
        [ [ NoFactsForPackage p
          | p <- NE.toList expected,
            p `notElem` present
          ],
          [FactsFromAnotherVersion | otherVersion],
          [ PackageDoesNotCover p ref
          | (p, refs) <- declared,
            ref <- refs,
            not (S.member ref (M.findWithDefault S.empty p covered))
          ],
          [ StoredModuleDidNotParse (moduleRefModule (storedModuleModuleRef m))
          | m <- modules,
            ParseFailed _ _ <- [storedModuleOutcome m]
          ],
          -- The package runs and this one are separate processes and can be
          -- given different rule sets. A suppression naming a rule this run does
          -- not run belongs to no level, so it would neither answer for anything
          -- nor be reported unused. Both ways of not running it, since the
          -- parser refuses either and this never saw the comment to say which.
          [ SuppressionNamesRuleNotRun (annotationFactRule a)
          | a <- annotations,
            useOf rs (annotationFactRule a) /= Just RuleRuns
          ],
          -- A build was given, so it answers for every module or it answers for
          -- none: a module it left out is one every rule is told nothing about,
          -- which reads exactly like a module there is nothing to say about.
          map NoArtifactFor uncovered
        ]
    )

moduleChecks :: RuleSet -> [ModuleContext -> CheckResult]
moduleChecks rs = [moduleCheckFindings c | r <- rulesAtLevel rs LevelModule, ModuleRule c <- [ruleImpl r]]

-- | Each level judges exactly the annotations naming its own rules, which keeps
-- unused-annotation detection sound without a global pass. A rule this set does
-- not run has no level, so its suppressions are reported by 'storeProblems' and
-- by the suppression parser instead.
annotationsAtLevel :: RuleSet -> Level -> [AnnotationFact] -> [AnnotationFact]
annotationsAtLevel rs l =
  filter ((== Just l) . fmap ruleLevel . ruleFor rs . annotationFactRule)

-- | Every suppression this package writes in a file no rule is run over.
--
-- Two kinds of file and one consequence. A file under a source directory that
-- no component claims is read by nothing, and a preprocessor's input holds no
-- Haskell until a build makes some. A rule that is never run over a file
-- reports nothing in it, so a suppression written in one answers for nothing
-- and always will. That is the verdict 'applySuppression' reaches for a
-- suppression whose rule ran and found nothing, and it is reported as the same
-- complaint.
--
-- No layer can reach these, because a layer judges the annotations of the
-- modules it read and neither kind of file is one of those. Both kinds are
-- judged here rather than one here and one wherever it happened to be noticed,
-- since what they have in common is the whole of what there is to say.
unreadSuppressionsOf :: RuleSet -> SourceRoot -> PackageModel -> IO Complaints
unreadSuppressionsOf rs root pm = do
  unclaimed <- unclaimedSourceFiles pm
  mconcat <$> traverse judged [(rp, f) | f <- unclaimed ++ preprocessed, Just rp <- [relPathIn root f]]
  where
    preprocessed :: [Path Abs File]
    preprocessed =
      [ moduleModelFile mm
      | c <- packageModelComponents pm,
        mm <- componentModelModules c,
        PreprocessedSource <- [moduleModelSource mm]
      ]

    judged :: (Path Rel File, Path Abs File) -> IO Complaints
    judged (rp, file) = do
      contents <- readSource file
      let (unused, problems) = unreadSuppressionsIn rs rp contents
      pure (complaintsOf (map ComplaintUnused unused ++ map ComplaintProblem problems))
