{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | One spec for every rule, so that adding a rule adds resources and no test
-- code, and one test for every resource, so that adding a case adds a file and
-- no test code either. It also carries the meta-properties, which therefore
-- cost nothing per rule.
module Hopinion.RuleSpec (spec) where

import Control.Monad (unless)
import Data.List (isPrefixOf, nub, sort, sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hopinion.Facts
import Hopinion.Project
import Hopinion.Report
import Hopinion.Report.Render
import Hopinion.Rule
import Hopinion.Rule.Gen (shippedRules)
import Hopinion.Rule.Id
import Hopinion.Rule.Registry (builtinRules)
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
import Path.IO (ensureDir, forgivingAbsence, listDirRel, makeAbsolute, withSystemTempDir)
import System.FilePath (dropTrailingPathSeparator)
import System.Process (readProcess)
import Test.Syd
import Text.Colour (TerminalCapabilities (..), renderChunksText)

-- | A directory per rule, named after it, holding the code that rule must and
-- must not report on. The name is a plain one now that ids are PascalCase with
-- nothing in them a directory name cannot hold.
resourceDir :: Path Rel Dir
resourceDir = [reldir|test_resources/Rule|]

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

spec :: Spec
spec = do
  -- Two rules by one name, or a name that is not an id, is a value that fails
  -- to be a set, and this is where the rules that ship are held to being one.
  it "ships a set of rules" $
    case ruleSet builtinRules [] of
      Right _ -> pure ()
      Left err -> expectationFailure (T.unpack (renderChunksText WithoutColours (renderRuleSetError err)))

  -- And this is what stops the rest of the suite passing over no rules when they
  -- are not: shippedRules falls back to the empty set, which every spec below
  -- would otherwise read as a rule that found nothing.
  it "runs the specs below over exactly the rules that ship" $
    map ruleId (ruleSetRules shippedRules) `shouldBe` map ruleId builtinRules

  -- Both directions at once: a rule with no resources fails here, and so does a
  -- directory left behind by a renamed or deleted rule.
  it "has one resource directory per rule and no others" $ do
    (dirs, files) <- listDirRel resourceDir
    expected <- traverse (ruleDirName . ruleId) (ruleSetRules shippedRules)
    sort dirs `shouldBe` sort expected
    files `shouldBe` []

  -- An id is what a suppression names, so it has to survive being written down
  -- and read back. Both directions, since a rule whose text no longer parses
  -- would take every suppression naming it with it.
  it "reads every rule id back from the text it is written as" $
    traverse (parseRuleId . ruleIdText . ruleId) (ruleSetRules shippedRules)
      `shouldBe` Just (map ruleId (ruleSetRules shippedRules))

  mapM_ ruleSpec (ruleSetRules shippedRules)

-- | A rule's own directory name, which is its id.
ruleDirName :: RuleId -> IO (Path Rel Dir)
ruleDirName = parseRelDir . T.unpack . ruleIdText

ruleSpec :: Rule -> Spec
ruleSpec r =
  describe (T.unpack (ruleIdText rid)) $ do
    dir <- runIO ((resourceDir </>) <$> ruleDirName rid)

    -- A rule with only clean resources would pass while never firing, and a
    -- rule with only dirty ones would pass while firing on everything, so both
    -- have to be there. Written as one assertion over the whole listing, so a
    -- resource named for neither fails here rather than sitting unread.
    it "has a clean case and a dirty one, and nothing else" $ do
      (dirs, files) <- listDirRel dir
      sort (nub (map caseOf dirs ++ map caseOf files))
        `shouldBe` [Just CleanCase, Just DirtyCase]

    case ruleLevel r of
      LevelModule -> scenarioDir dir (moduleScenario rid . (dir </>))
      LevelPackage -> projectScenarios rid dir SplitIsNotUnderTest
      LevelProject -> projectScenarios rid dir SplitIsUnderTest
  where
    rid = ruleId r

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
        goldenTextFile (toFilePath golden) (renderFindings <$> findingsInModule rid file)

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

      -- A rule reads structure, so reformatting the code must not change what it
      -- says about it. The spans move and the messages do not, so the messages
      -- are what is compared.
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
      goldenTextFile (toFilePath golden) (renderFindings <$> findingsInProject rid project)

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

-- | Every finding as one line: which rule, where, what it is about, and what it
-- says.
--
-- The scope is in there because it is what a suppression is matched on, so a
-- rule that points at the right line and names the wrong declaration is a rule
-- whose suppression cannot be written. Sorted, because the order findings
-- arrive in is the order a query returned them.
renderFindings :: [Finding] -> Text
renderFindings fs =
  T.unlines
    ( sort
        [ T.intercalate
            "\t"
            [ ruleIdText (findingRule f),
              spanText (findingSpan f),
              scopeText (findingScope f),
              findingMessage f
            ]
        | f <- fs
        ]
    )
  where
    scopeText sk = case sk of
      ScopeOfFile m -> moduleRefText m
      ScopeOfDecl m d -> T.concat [moduleRefText m, " ", declNameText d]

findingsInModule :: RuleId -> Path Rel File -> IO [Finding]
findingsInModule rid file = do
  report <- runModuleCommand shippedRules file [] ComponentLib
  pure [f | f <- complaintsFindings report, findingRule f == rid]

findingsInProject :: RuleId -> Path Rel Dir -> IO [Finding]
findingsInProject rid dir = do
  report <- runCheck shippedRules noHieDirectories =<< rootAt dir
  [t | ComplaintFailure t <- complaintList report] `shouldBe` []
  pure [f | f <- complaintsFindings report, findingRule f == rid]

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
      -- what the package phase already found, so the two paths agree or they
      -- do not.
      sortedFindings throughFiles `shouldBe` sortedFindings inOneProcess

-- | What the package command writes, written the same way, so that the layered
-- path under test is the one the derivations run.
writeOne :: Path Abs Dir -> SourceRoot -> PackageModel -> IO (T.Text, Path Abs Dir)
writeOne tmp root pm = do
  let name = packageNameText (packageModelName pm)
  dir <- (tmp </>) <$> parseRelDir (T.unpack name)
  ensureDir dir
  report <- runPackageCommand shippedRules noHieDirectories root (packageModelDir pm) (Just dir)
  (sources, _) <- sourcesForReport [root] report
  writeReportTo shippedRules dir sources report
  pure (name, dir)

sortedFindings :: Complaints -> [Finding]
sortedFindings report =
  sortOn
    (\f -> (findingSpan f, ruleIdText (findingRule f), findingMessage f))
    (complaintsFindings report)

-- | A source root over a directory, resolved against the working directory the
-- suite runs in, which is the package directory.
rootAt :: Path Rel Dir -> IO SourceRoot
rootAt dir = do
  absDir <- makeAbsolute dir
  pure SourceRoot {sourceRootDir = absDir, sourceRootPrefix = Nothing}
