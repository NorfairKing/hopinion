{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The helpers that build specs, which is the one kind of test helper that is
-- better as a function than as a let-binding.
--
-- A spec-building helper is what makes a suite where adding a rule adds
-- resources and no test code, so it is worth naming, worth a type signature and
-- worth reading on its own. What it may not be is a top-level binding in a test
-- file, where nothing can reach it and nothing distinguishes it from a helper
-- that hides what a test asserts. Here it is neither: a module of its own that
-- the test files import.
module Hopinion.TestUtils
  ( resourcesDir,
    specDir,
    ownedSpec,
    ruleResourcesDir,
    ruleSpec,
  )
where

import Control.Monad (unless)
import Data.List (isPrefixOf, nub, sort, sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hopinion.Facts.Component
import Hopinion.Facts.Name
import Hopinion.Project
import Hopinion.Report
import Hopinion.Report.Render
import Hopinion.Rule
import Hopinion.Rule.Gen (shippedRules)
import Hopinion.Rule.Id
import Hopinion.Run
import Path
  ( Abs,
    Dir,
    File,
    Path,
    Rel,
    addExtension,
    dirname,
    fileExtension,
    filename,
    parent,
    parseRelDir,
    parseRelFile,
    reldir,
    toFilePath,
    (</>),
  )
import Path.IO (ensureDir, forgivingAbsence, getCurrentDir, listDirRel, makeAbsolute, withSystemTempDir)
import System.FilePath (dropTrailingPathSeparator)
import System.Process (readProcess)
import Test.Syd

-- | Everything any spec reads, which is what 'ownedSpec' holds to belonging to
-- exactly one of them.
resourcesDir :: Path Rel Dir
resourcesDir = [reldir|test_resources|]

specDir :: Path Rel Dir
specDir = [reldir|test/Hopinion|]

-- | One directory of resources, and the spec that has to be reading it.
ownedSpec :: [Path Rel File] -> Path Rel Dir -> Spec
ownedSpec specs entry =
  -- A directory's name and the first half of its spec's name are the same word,
  -- and the separator a directory's rendering ends in is the only thing between
  -- them.
  let name :: String
      name = dropTrailingPathSeparator (toFilePath (dirname entry))
   in it (unwords [name, concat ["belongs to Hopinion.", name, "Spec"]]) $ do
        expected <- parseRelFile (concat [name, "Spec.hs"])
        if expected `elem` specs
          then pure ()
          else
            expectationFailure
              ( unwords
                  [ "The resources in",
                    toFilePath (resourcesDir </> entry),
                    "have no spec.",
                    "Either read them from",
                    toFilePath (specDir </> expected),
                    "or delete them."
                  ]
              )

-- | A directory per rule, named after it, holding the code that rule must and
-- must not report on. The name is a plain one now that ids are PascalCase with
-- nothing in them a directory name cannot hold.
ruleResourcesDir :: Path Rel Dir
ruleResourcesDir = [reldir|test_resources/Rule|]

-- | What a resource in a rule's directory is for: code the rule must stay quiet
-- about, or code it must report on. A name that says neither is an error, so
-- nothing can sit in a rule's directory without a test over it.
data ResourceCase
  = CleanCase
  | DirtyCase
  deriving stock (Show, Eq, Ord)

-- | Read off the name alone, so it answers the same for the directory of a
-- project case, the module of a module case, and the golden beside either.
--
-- A prefix test, which is what lets a directory's name be asked without its
-- trailing separator getting in the way.
caseOf :: Path Rel t -> Maybe ResourceCase
caseOf name
  | "good" `isPrefixOf` toFilePath name = Just CleanCase
  | "bad" `isPrefixOf` toFilePath name = Just DirtyCase
  | otherwise = Nothing

-- | Whether this rule's level makes the layered path meaningful over the same
-- resources.
data SplitCheck
  = SplitIsUnderTest
  | SplitIsNotUnderTest

-- | Everything one rule is held to: that it has both kinds of resource, and
-- what it says about each of them.
ruleSpec :: Rule -> Spec
ruleSpec r =
  let rid = ruleId r
   in describe (T.unpack (ruleIdText rid)) $ do
        dir <- runIO ((ruleResourcesDir </>) <$> ruleDirName rid)

        -- A rule with only clean resources would pass while never firing, and a
        -- rule with only dirty ones would pass while firing on everything, so
        -- both have to be there. Written as one assertion over the whole
        -- listing, so a resource named for neither fails here rather than
        -- sitting unread.
        it "has a clean case and a dirty one, and nothing else" $ do
          (dirs, files) <- listDirRel dir
          sort (nub (map caseOf dirs ++ map caseOf files))
            `shouldBe` [Just CleanCase, Just DirtyCase]

        case ruleLevel r of
          LevelModule -> scenarioDir dir (moduleScenario rid . (dir </>))
          LevelPackage -> projectScenarios rid dir SplitIsNotUnderTest
          LevelProject -> projectScenarios rid dir SplitIsUnderTest

-- | A rule's own directory name, which is its id.
ruleDirName :: RuleId -> IO (Path Rel Dir)
ruleDirName = parseRelDir . T.unpack . ruleIdText

-- | One module, and beside it the golden of what the rule makes of it.
--
-- The golden rather than only its emptiness, because "reports something" is
-- satisfied by a rule that reports the wrong line, about the wrong declaration,
-- with the wrong sentence in it, and the suppression a reader is offered is
-- built out of two of those.
moduleScenario :: RuleId -> Path Rel File -> Spec
moduleScenario rid file
  | fileExtension file /= Just ".hs" =
      it "is a golden belonging to a case beside it" $ do
        fileExtension file `shouldBe` Just ".golden"
        caseOf (filename file) `shouldNotBe` Nothing
  | otherwise = do
      golden <- runIO (addExtension ".golden" file)

      it "reports what the golden says" $
        goldenTextFile (toFilePath golden) (renderedFindingsInModule rid file)

      case caseOf (filename file) of
        Just CleanCase ->
          it "reports nothing" $ do
            fs <- findingsInModule rid file
            fs `shouldBe` []
        Just DirtyCase ->
          it "reports something" $ do
            fs <- findingsInModule rid file
            fs `shouldNotBe` []
        Nothing ->
          it
            "is named for what it is a case of"
            (expectationFailure (unwords ["Neither a good nor a bad case:", toFilePath file]) :: IO ())

      -- A rule reads structure, so reformatting the code must not change what
      -- it says about it. The spans move and the messages do not, so the
      -- messages are what is compared.
      unless (filename file `elem` formatSensitive) $
        it "says the same thing after ormolu" $ do
          before' <- map findingMessage <$> findingsInModule rid file
          original <- TIO.readFile (toFilePath file)
          formatted <-
            T.pack
              <$> readProcess "ormolu" ["--stdin-input-file", toFilePath file] (T.unpack original)
          facts <- factsForSource shippedRules file [] ComponentLib formatted
          let after' =
                [ findingMessage f
                | f <- complaintsFindings (runModuleLayer shippedRules facts),
                  findingRule f == rid
                ]
          after' `shouldBe` before'

-- | Resources whose layout is the thing under test, so ormolu would be
-- rewriting the case rather than reformatting it. Empty, and kept because the
-- comment rules that are coming are the ones that will need it.
formatSensitive :: [Path Rel File]
formatSensitive = []

-- | A rule above the module level needs a whole repository per case rather than
-- a module, and @scenarioDir@ enumerates files, so the enumeration is here.
projectScenarios :: RuleId -> Path Rel Dir -> SplitCheck -> Spec
projectScenarios rid dir splitCheck = do
  entries <- runIO (subdirectoriesOf dir)
  mapM_ (projectScenario rid splitCheck . (dir </>)) entries

projectScenario :: RuleId -> SplitCheck -> Path Rel Dir -> Spec
projectScenario rid splitCheck project =
  describe (toFilePath (dirname project)) $ do
    golden <- runIO (goldenBeside project)

    it "reports what the golden says" $
      goldenTextFile (toFilePath golden) (renderedFindingsInProject rid project)

    case caseOf (dirname project) of
      Just CleanCase ->
        it "reports nothing" $ do
          fs <- findingsInProject rid project
          fs `shouldBe` []
      Just DirtyCase ->
        it "reports something" $ do
          fs <- findingsInProject rid project
          fs `shouldNotBe` []
      Nothing ->
        it
          "is named for what it is a case of"
          (expectationFailure (unwords ["Neither a good nor a bad case:", toFilePath project]) :: IO ())

    case splitCheck of
      SplitIsNotUnderTest -> pure ()
      SplitIsUnderTest ->
        it "reports the same through fact files as in one process" (splitAgrees project)

-- | The scenarios, which are the directories: a rule above the module level is
-- given a repository per case, and the goldens beside them are files.
--
-- Empty rather than an exception when the directory is absent, so a rule whose
-- resources are missing fails the listing test with a message rather than
-- taking the suite down at definition time.
--
-- One listing rather than a question per entry: 'listDirRel' already separates
-- the directories from the files, so nothing here has to ask about each one and
-- get an answer that was true a moment ago.
subdirectoriesOf :: Path Rel Dir -> IO [Path Rel Dir]
subdirectoriesOf dir = do
  listed <- forgivingAbsence (listDirRel dir)
  pure $ case listed of
    Nothing -> []
    Just (dirs, _) -> sort dirs

-- | The golden for a project case, which is a file beside the directory rather
-- than inside it, so that adding a case adds a directory and a file and no test
-- code.
--
-- The separator has to come off by hand: a directory's name and a file's name
-- are the same word here, and @path@ has nothing that reads one as the other.
goldenBeside :: Path Rel Dir -> IO (Path Rel File)
goldenBeside project = do
  name <- parseRelFile (dropTrailingPathSeparator (toFilePath (dirname project)))
  addExtension ".golden" (parent project </> name)

-- | The findings drawn against the code they point at, which is the rendering a
-- person is shown at the end of a run.
--
-- Spans and messages alone did not review: reading @8:1-9:36@ off a golden and
-- resolving it against the resource by hand is work nobody does, so a rule
-- underlining half of the instance it named read the same as one underlining
-- all of it. Drawn, the wrong span is the wrong code with a line under it.
--
-- What this no longer pins is the scope key, which is what a suppression is
-- matched on and which the rendering does not show. The hint under each finding
-- names the place a suppression has to go, which is the half of that a reader
-- can act on.
--
-- Sorted by span, because the order findings arrive in is the order a query
-- returned them.
renderedFindings :: SourceRoot -> [Finding] -> IO Text
renderedFindings root fs = do
  let report = complaintsOf (map ComplaintFinding (sortOn findingSpan fs))
  (sources, missing) <- sourcesForReport [root] report
  pure (renderReportColoured shippedRules sources (report <> missing))

findingsInModule :: RuleId -> Path Rel File -> IO [Finding]
findingsInModule rid file = do
  report <- runModuleCommand shippedRules file [] ComponentLib
  pure [f | f <- complaintsFindings report, findingRule f == rid]

-- | A module case names its resource the way the suite was invoked, relative to
-- the package directory, so that directory is the root its findings are read
-- against.
renderedFindingsInModule :: RuleId -> Path Rel File -> IO Text
renderedFindingsInModule rid file = do
  here <- getCurrentDir
  let root = SourceRoot {sourceRootDir = here, sourceRootPrefix = Nothing}
  renderedFindings root =<< findingsInModule rid file

findingsInProject :: RuleId -> Path Rel Dir -> IO [Finding]
findingsInProject rid dir = do
  report <- runCheck shippedRules noHieDirectories =<< rootAt dir
  [t | ComplaintFailure t <- complaintList report] `shouldBe` []
  pure [f | f <- complaintsFindings report, findingRule f == rid]

renderedFindingsInProject :: RuleId -> Path Rel Dir -> IO Text
renderedFindingsInProject rid dir = do
  root <- rootAt dir
  renderedFindings root =<< findingsInProject rid dir

-- | The property that keeps the split honest: running a project rule through
-- fact files on disk must find exactly what running it in one process finds.
--
-- It catches the class of bug the split introduces, where a project rule reads
-- a fact that is in memory in the all-in-one path and that extraction never
-- serialises. Per project rule, so a rule added later is covered without anyone
-- remembering to add it anywhere.
splitAgrees :: Path Rel Dir -> IO ()
splitAgrees dir = withSystemTempDir "hopinion-split" $ \tmp -> do
  root <- rootAt dir
  inOneProcess <- runCheck shippedRules noHieDirectories root
  eModels <- discoverPackages root (sourceRootDir root)
  case eModels of
    Left err -> expectationFailure (T.unpack (renderDiscoveryError err))
    Right models -> do
      named <- mapM (writeOne tmp root) models
      throughFiles <- runProjectCommand shippedRules noHieDirectories (map snd named) (map fst named) Nothing
      [t | ComplaintFailure t <- complaintList throughFiles] `shouldBe` []
      -- Every finding, not only the project ones: the project phase unions in
      -- what the package phase already found, so the two paths agree or they do
      -- not.
      sortedFindings throughFiles `shouldBe` sortedFindings inOneProcess

-- | What the package command writes, written the same way, so that the layered
-- path under test is the one the derivations run.
writeOne :: Path Abs Dir -> SourceRoot -> PackageModel -> IO (Text, Path Abs Dir)
writeOne tmp root pm = do
  let name = packageNameText (packageModelName pm)
  dir <- (tmp </>) <$> parseRelDir (T.unpack name)
  ensureDir dir
  report <- runPackageCommand shippedRules noHieDirectories root (packageModelDir pm) (Just dir)
  (sources, _) <- sourcesForReport [root] report
  writeReportTo shippedRules dir sources report
  pure (name, dir)

sortedFindings :: Complaints -> [Finding]
sortedFindings =
  sortOn
    (\f -> (findingSpan f, ruleIdText (findingRule f), findingMessage f))
    . complaintsFindings

-- | A source root over a directory, resolved against the working directory the
-- suite runs in, which is the package directory.
rootAt :: Path Rel Dir -> IO SourceRoot
rootAt dir = do
  absDir <- makeAbsolute dir
  pure SourceRoot {sourceRootDir = absDir, sourceRootPrefix = Nothing}
