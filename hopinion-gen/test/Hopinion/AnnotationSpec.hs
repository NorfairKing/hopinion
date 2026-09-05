{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hopinion.AnnotationSpec (spec) where

import Control.Monad (forM)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Hopinion.Annotation
import Hopinion.Comment
import Hopinion.Facts.Name
import Hopinion.Facts.Place
import Hopinion.Facts.Suppression
import Hopinion.Project
import Hopinion.Report
import Hopinion.Report.Render
import Hopinion.Rule
import Hopinion.Rule.Gen (shippedRules)
import Hopinion.Rule.Id
import Hopinion.Rule.Registry (builtinRules)
import Hopinion.Run
import Path (Dir, File, Path, Rel, mkRelFile, parseRelDir, reldir, relfile, toFilePath, (</>))
import Path.IO (ensureDir, listDirRel, makeAbsolute, withSystemTempDir)
import Test.Syd
import Text.Colour (TerminalCapabilities (..), renderChunksText)

-- | One miniature repository per thing a suppression can be doing: answering
-- for a finding, no longer relevant, or written somewhere it cannot attach.
-- Each one beside the golden of what the tool makes of it.
resourceDir :: Path Rel Dir
resourceDir = [reldir|test_resources/Annotation|]

spec :: Spec
spec = do
  it "has a project for each of the three states a suppression can be in" $ do
    (dirs, files) <- listDirRel resourceDir
    sort dirs
      `shouldBe` [ [reldir|misplaced|],
                   [reldir|self-weeding|],
                   [reldir|suppressed|]
                 ]
    sort files
      `shouldBe` [ [relfile|misplaced.golden|],
                   [relfile|self-weeding.golden|]
                 ]

  -- What the marker test lets through, which decides what is held to being a
  -- suppression at all. A bare @[allow]@ is one and is refused for having no
  -- rule; a word that merely starts the same way is not one, and reporting it
  -- would fail a run over an ordinary comment.
  describe "annotationsOf" $ do
    it "reads a bare marker as a suppression that is missing its rule" $
      let (facts, problems) = annotationsOf shippedRules exampleModule [commentSaying "[allow] because"]
       in (length facts, map annotationProblemMessage problems)
            `shouldBe` (0, [renderAnnotationError NoRuleNamed])

    it "leaves a comment that merely starts like the marker alone" $
      annotationsOf shippedRules exampleModule [commentSaying "[allowlist] the hosts we permit"]
        `shouldBe` ([], [])

  describe "parseAnnotation" $ do
    it "rejects a bare marker with no rule" $
      parsed "[allow] because"
        `shouldBe` Left NoRuleNamed
    it "rejects an unknown rule" $
      parsed "[allow:NoSuchRule] because"
        `shouldBe` Left (UnknownRuleId "NoSuchRule")
    it "rejects a suppression with no reason" $
      parsed "[allow:CommentBareTodo]"
        `shouldBe` Left NoReason
    it "rejects a comma list of rules" $
      parsed "[allow:CommentBareTodo,HsGenValidInGenPackage] because"
        `shouldBe` Left TwoRulesAtOneSite
    it "refuses to sit in a documentation comment" $
      parsedIn StyleHaddockNext "[allow:CommentBareTodo] because"
        `shouldBe` Left InHaddock
    it "refuses to be unattached" $
      parsedWith Unattached StyleLine "[allow:CommentBareTodo] because"
        `shouldBe` Left AttachedToNothing
    it "accepts a rule and a reason" $ do
      reason <- givenReason "because it is fine"
      parsed "[allow:CommentBareTodo] because it is fine"
        `shouldBe` Right
          AnnotationFact
            { annotationFactRule = RuleId "CommentBareTodo",
              annotationFactScope = ScopeOfDecl exampleModule (DeclName "loose"),
              annotationFactPrecision = PrecisionDecl,
              annotationFactReason = ReasonGiven reason,
              annotationFactSpan = spanAt 1
            }
    it "records the adoption reason as its own case" $
      parsed (T.concat ["[allow:CommentBareTodo] ", adoptionReasonText])
        `shouldBe` Right
          AnnotationFact
            { annotationFactRule = RuleId "CommentBareTodo",
              annotationFactScope = ScopeOfDecl exampleModule (DeclName "loose"),
              annotationFactPrecision = PrecisionDecl,
              annotationFactReason = ReasonAdoption,
              annotationFactSpan = spanAt 1
            }

  -- A rule that is never run over a file reports nothing in it, so every
  -- suppression written in one answers for nothing. The verdict is the same one
  -- 'applySuppression' reaches, and these assert that it is reached by the same
  -- steps: what counts as a suppression, and what counts as naming a rule.
  describe "unreadSuppressionsIn" $ do
    it "reports a suppression naming a rule as unused, from its marker to the end of the line" $
      unreadSuppressionsIn shippedRules exampleFile "module Thing where\n\n-- [allow:CommentBareTodo] because\n"
        `shouldBe` ( [ Unused
                         { unusedRule = RuleId "CommentBareTodo",
                           unusedSpan =
                             Span
                               { spanFile = exampleFile,
                                 spanStart = Position {positionLine = 3, positionCol = 4},
                                 spanEnd = Position {positionLine = 3, positionCol = 35}
                               }
                         }
                     ],
                     []
                   )
    it "reports each of two suppressions rather than the file holding them" $
      map (positionLine . spanStart . unusedSpan) (fst (unreadSuppressionsIn shippedRules exampleFile "-- [allow:CommentBareTodo] one\nx = 1\n-- [allow:HsNoFilePath] two\n"))
        `shouldBe` [1, 3]
    it "reports one that names no rule as broken, exactly as a file that is read would" $
      map annotationProblemMessage (snd (unreadSuppressionsIn shippedRules exampleFile "-- [allow] because\n"))
        `shouldBe` [renderAnnotationError NoRuleNamed]
    it "reports one that names a rule nothing answers to as broken" $
      map annotationProblemMessage (snd (unreadSuppressionsIn shippedRules exampleFile "-- [allow:NoSuchRule] because\n"))
        `shouldBe` [renderAnnotationError (UnknownRuleId "NoSuchRule")]
    it "leaves a file that merely mentions a word starting the same way alone" $
      unreadSuppressionsIn shippedRules exampleFile "module Thing where\n\n-- the [allowlist] of hosts we permit\n"
        `shouldBe` ([], [])
    it "passes over such a word to the suppression written after it" $
      map (positionCol . spanStart . unusedSpan) (fst (unreadSuppressionsIn shippedRules exampleFile "-- the [allowlist], and [allow:CommentBareTodo] because\n"))
        `shouldBe` [25]

  -- The text the report offers a reader comes from here, so a suppression this
  -- suggests and 'parseAnnotation' then rejects would be worse than no
  -- suggestion at all.
  --
  -- Which form is needed turns on two independent things, so there is a case
  -- for each: a comment above a module header has a real span and a file scope,
  -- and a generated instance has a declaration scope and no line to point at.
  describe "suppressionFor" $ do
    it "is site-scoped for a finding about code" $
      suppressionFor todoFinding {findingSpan = spanAt 7} "because"
        `shouldBe` "-- [allow:CommentBareTodo] because"
    it "is file-scoped for a finding about a whole file" $
      suppressionFor todoFinding {findingSpan = wholeFileSpan exampleFile} "because"
        `shouldBe` "-- [allow:file:CommentBareTodo] because"
    it "is file-scoped for a finding whose comment attaches to no declaration" $
      suppressionFor todoFinding {findingScope = ScopeOfFile exampleModule} "because"
        `shouldBe` "-- [allow:file:CommentBareTodo] because"

  describe "applySuppression" $ do
    it "leaves an unannotated finding alone" $
      applySuppression [] [todoFinding]
        `shouldBe` Suppression
          { suppressionRemaining = [todoFinding],
            suppressionUnused = [],
            suppressionOverBroad = []
          }
    it "reports an annotation that suppresses nothing" $
      withParsed "[allow:CommentBareTodo] because" $ \a ->
        applySuppression [a] []
          `shouldBe` Suppression
            { suppressionRemaining = [],
              suppressionUnused = [Unused {unusedRule = RuleId "CommentBareTodo", unusedSpan = annotationFactSpan a}],
              suppressionOverBroad = []
            }
    it "reports an annotation that suppresses more than one finding" $
      withParsed "[allow:CommentBareTodo] because" $ \a ->
        applySuppression [a] [todoFinding, todoFinding]
          `shouldBe` Suppression
            { suppressionRemaining = [],
              suppressionUnused = [],
              suppressionOverBroad = [OverBroad {overBroadAnnotation = a, overBroadCount = 2}]
            }

    -- Two suppressions and two findings, each suppression nearest one finding.
    -- Both answer for something, so neither is unused and neither is over-broad.
    --
    -- The nearest one has to win for that to come out, and a line number is a
    -- 'Word': the distance cannot be a subtraction, because one taken from the
    -- other is not a small negative but a number larger than any file.
    it "pairs each suppression with the finding nearest it" $ do
      let annotationAt line = (\a -> a {annotationFactSpan = spanAt line}) <$> parsed "[allow:CommentBareTodo] because"
      case (annotationAt 9, annotationAt 12) of
        (Right above, Right below) ->
          applySuppression
            [above, below]
            [todoFinding {findingSpan = spanAt 10}, todoFinding {findingSpan = spanAt 11}]
            `shouldBe` Suppression
              { suppressionRemaining = [],
                suppressionUnused = [],
                suppressionOverBroad = []
              }
        _ -> expectationFailure "The spec's own annotations do not parse."

  -- Written suppressions, read back and matched against the findings they
  -- answer for. Between them the modules cover both forms: the site-scoped one
  -- against a declaration, and the file-scoped one for the placements where a
  -- comment attaches to no declaration, which is what the report offers for
  -- such a finding.
  describe "suppressed" $
    it "is clean, because every finding in it is suppressed" $ do
      report <-
        runCheck shippedRules noHieDirectories =<< rootAt (resourceDir </> [reldir|suppressed|])
      report `shouldBe` mempty

  -- The package runs and the project run are separate processes, so they can be
  -- given different rule sets. A suppression naming a rule the project run does
  -- not run belongs to no level: it answers for nothing and is reported unused
  -- by nobody, arriving by the one door that does not go past the parser.
  describe "a rule set that changed between the two halves of a run" $
    it "reports a suppression in the facts naming a rule the project run has turned off" $
      withSystemTempDir "hopinion-turned-off" $ \tmp -> do
        root <- rootAt (resourceDir </> [reldir|suppressed|])
        withEverything <-
          either
            (expectationFailure . T.unpack . renderChunksText WithoutColours . renderRuleSetError)
            pure
            (ruleSet builtinRules [])
        withoutTodo <-
          either
            (expectationFailure . T.unpack . renderChunksText WithoutColours . renderRuleSetError)
            pure
            (ruleSet builtinRules [RuleId "CommentBareTodo"])
        eModels <- discoverPackages root (sourceRootDir root)
        case eModels of
          Left err -> expectationFailure (T.unpack (renderDiscoveryError err))
          Right models -> do
            named <- forM models $ \pm -> do
              let name = packageNameText (packageModelName pm)
              dir <- (tmp </>) <$> parseRelDir (T.unpack name)
              ensureDir dir
              report <- runPackageCommand withEverything noHieDirectories root (packageModelDir pm) (Just dir)
              (sources, _) <- sourcesForReport [root] report
              writeReportTo withEverything dir sources report
              pure (name, dir)
            report <-
              runProjectCommand
                withoutTodo
                noHieDirectories
                (map snd named)
                (map fst named)
                Nothing
            [t | ComplaintFailure t <- complaintList report]
              `shouldBe` replicate
                3
                (FactsIncomplete (SuppressionNamesRuleNotRun (RuleId "CommentBareTodo")))

  -- The keystone. On-by-default with unlimited local escapes is only safe while
  -- a suppression cannot outlive its reason, so every way one can stop being
  -- relevant has to fail the run. The golden is the list of ways, and the one
  -- suppression in that module which does answer for a finding is absent.
  --
  -- A file no rule is ever run over is one of those ways and the quietest of
  -- them, since nothing there is parsed and so nothing there is judged. Both
  -- kinds are in the repository: a file no component claims, and a preprocessor
  -- input whose Haskell only exists at build time.
  describe "self-weeding" $ do
    -- What kind of complaint it is lives in the golden below; here it is only
    -- that there is one, which is what a wrapper deciding whether to fail asks.
    it "fails the run" $ do
      report <- runCheck shippedRules noHieDirectories =<< rootAt selfWeeding
      isClean report `shouldBe` False

    it "reports every suppression that has stopped being relevant" $
      goldenTextFile
        (toFilePath (resourceDir </> [relfile|self-weeding.golden|]))
        (renderedReportFor selfWeeding)

  -- A suppression can also fail by being somewhere it cannot attach, which is
  -- not the same as being irrelevant: it answers for nothing because it reaches
  -- nothing. One module per placement where that happens, so a change to comment
  -- attachment cannot quietly turn one into a suppression that covers more than
  -- whoever wrote it meant.
  describe "misplaced" $ do
    it "fails the run" $ do
      report <- runCheck shippedRules noHieDirectories =<< rootAt misplaced
      isClean report `shouldBe` False

    it "reports a site-scoped suppression in each place one cannot attach" $
      goldenTextFile
        (toFilePath (resourceDir </> [relfile|misplaced.golden|]))
        (renderedReportFor misplaced)

selfWeeding :: Path Rel Dir
selfWeeding = resourceDir </> [reldir|self-weeding|]

misplaced :: Path Rel Dir
misplaced = resourceDir </> [reldir|misplaced|]

-- | What a person would see, which is what the goldens are of.
renderedReportFor :: Path Rel Dir -> IO Text
renderedReportFor dir = do
  root <- rootAt dir
  report <- runCheck shippedRules noHieDirectories root
  (sources, missing) <- sourcesForReport [root] report
  pure (renderReport shippedRules sources (report <> missing))

-- | A source root over a directory, resolved against the working directory the
-- suite runs in, which is the package directory.
rootAt :: Path Rel Dir -> IO SourceRoot
rootAt dir = do
  absDir <- makeAbsolute dir
  pure SourceRoot {sourceRootDir = absDir, sourceRootPrefix = Nothing}

todoFinding :: Finding
todoFinding =
  Finding
    { findingRule = RuleId "CommentBareTodo",
      findingScope = ScopeOfDecl exampleModule (DeclName "loose"),
      findingSpan = spanAt 3,
      findingMessage = "a bare marker"
    }

exampleModule :: ModuleRef
exampleModule = ModuleRef {moduleRefComponent = ComponentName "lib", moduleRefModule = ModuleKey "Thing"}

-- | A span of a whole line, which is the shape a real one has: GHC gives an
-- extent, and a span that starts and ends in one place is the encoding of a
-- whole file rather than of anything in one. See 'isWholeFileSpan'.
spanAt :: Word -> Span
spanAt line =
  Span
    { spanFile = exampleFile,
      spanStart = Position {positionLine = line, positionCol = 1},
      spanEnd = Position {positionLine = line, positionCol = 40}
    }

-- | A line comment carrying this text, attached to a declaration.
commentSaying :: Text -> CommentFact
commentSaying text =
  CommentFact
    { commentFactSpan = spanAt 3,
      commentFactStyle = StyleLine,
      commentFactText = text,
      commentFactAttachment = AttachedToDecl (DeclName "loose")
    }

-- | A path spelled out here rather than parsed, so that a spec asserting on a
-- span is not also asserting that the parser works.
exampleFile :: Path Rel File
exampleFile = $(mkRelFile "thing/src/Thing.hs")

-- | The reasons here are written as literals, so a failure is a test that
-- spells an empty one rather than anything about the tool.
givenReason :: Text -> IO NonEmptyText
givenReason t =
  maybe (expectationFailure (unwords ["Empty reason in this spec:", T.unpack t])) pure (nonEmptyText t)

parsed :: Text -> Either AnnotationError AnnotationFact
parsed = parsedIn StyleLine

parsedIn :: CommentStyle -> Text -> Either AnnotationError AnnotationFact
parsedIn = parsedWith (AttachedToDecl (DeclName "loose"))

parsedWith :: Attachment -> CommentStyle -> Text -> Either AnnotationError AnnotationFact
parsedWith attachment style text =
  parseAnnotation
    shippedRules
    exampleModule
    CommentFact
      { commentFactSpan = spanAt 1,
        commentFactStyle = style,
        commentFactText = text,
        commentFactAttachment = attachment
      }

withParsed :: Text -> (AnnotationFact -> IO ()) -> IO ()
withParsed text act = case parsed text of
  Left err -> expectationFailure (T.unpack (renderAnnotationError err))
  Right a -> act a
