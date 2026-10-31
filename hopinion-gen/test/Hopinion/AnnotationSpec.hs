{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hopinion.AnnotationSpec (spec) where

import Control.Monad (forM)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Hopinion.Annotation
import qualified Hopinion.Check.Comment.BareTodo.Rule as BareTodo
import Hopinion.Comment
import Hopinion.Facts.Gen ()
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
import Path (Dir, Path, Rel, parseRelDir, reldir, relfile, toFilePath, (</>))
import Path.IO (ensureDir, listDirRel, makeAbsolute, withSystemTempDir)
import Test.QuickCheck (forAll, suchThat)
import Test.Syd
import Test.Syd.Validity (GenValid (..), forAllValid)
import Text.Colour (TerminalCapabilities (..), renderChunksText)

spec :: Spec
spec = do
  -- One miniature repository per thing a suppression can be doing: answering
  -- for a finding, no longer relevant, or written somewhere it cannot attach.
  -- Each one beside the golden of what the tool makes of it.
  let resourceDir :: Path Rel Dir
      resourceDir = [reldir|test_resources/Annotation|]

  let selfWeeding :: Path Rel Dir
      selfWeeding = resourceDir </> [reldir|self-weeding|]

  let misplaced :: Path Rel Dir
      misplaced = resourceDir </> [reldir|misplaced|]

  -- A source root over a directory, resolved against the working directory the
  -- suite runs in, which is the package directory.
  let rootAt :: Path Rel Dir -> IO SourceRoot
      rootAt dir = do
        absDir <- makeAbsolute dir
        pure
          SourceRoot
            { sourceRootDir = absDir,
              sourceRootPrefix = Nothing
            }

  -- What a person would see, which is what the goldens are of.
  let renderedReportFor :: Path Rel Dir -> IO Text
      renderedReportFor dir = do
        root <- rootAt dir
        report <- runCheck shippedRules noHieDirectories root
        (sources, missing) <- sourcesForReport [root] report
        pure (renderReportColoured shippedRules sources (report <> missing))

  -- A comment with this text in it, written as a line comment above a
  -- declaration, which is the one place every suppression here can attach.
  let lineCommentSaying :: DeclName -> Text -> CommentFact -> CommentFact
      lineCommentSaying d text cf =
        cf
          { commentFactStyle = CommentStyleLine,
            commentFactText = text,
            commentFactAttachment = AttachmentToDecl d
          }

  let parsedWith :: ModuleRef -> Span -> Attachment -> CommentStyle -> Text -> Either AnnotationError AnnotationFact
      parsedWith mk sp attachment style text =
        parseAnnotation
          shippedRules
          mk
          CommentFact
            { commentFactSpan = sp,
              commentFactStyle = style,
              commentFactText = text,
              commentFactAttachment = attachment
            }

  -- A reason a suppression is read back as written: one with no space around
  -- it to strip, and not the one that means the suppression came with adoption.
  let isPlainReason :: NonEmptyText -> Bool
      isPlainReason r =
        let t = nonEmptyTextText r
         in T.strip t == t && t /= adoptionReasonText

  -- The same span on another line, for the tests that are about which line is
  -- nearest which.
  let onLine :: Word -> Span -> Span
      onLine line sp =
        sp
          { spanStart = (spanStart sp) {positionLine = line},
            spanEnd = (spanEnd sp) {positionLine = line}
          }

  -- A finding this annotation answers for, whatever its precision is.
  let answeredBy :: AnnotationFact -> Finding -> Finding
      answeredBy a f =
        f
          { findingRule = annotationFactRule a,
            findingScope = annotationFactScope a,
            findingSpan = case annotationFactPrecision a of
              AnnotationPrecisionStatement sp -> sp
              AnnotationPrecisionDecl -> annotationFactSpan a
              AnnotationPrecisionFile -> annotationFactSpan a
          }

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
    -- [allow:TestNoDummyValues] The bare marker is the case under test.
    it "reads a bare marker as a suppression that is missing its rule" $
      forAllValid $ \(mk, d, cf) ->
        let (facts, problems) = annotationsOf shippedRules mk [lineCommentSaying d "[allow] because" cf]
         in (length facts, map annotationProblemMessage problems)
              `shouldBe` (0, [renderAnnotationError AnnotationErrorNoRuleNamed])

    -- [allow:TestNoDummyValues] A word that starts like the marker is the case under test.
    it "leaves a comment that merely starts like the marker alone" $
      forAllValid $ \(mk, d, cf) ->
        annotationsOf shippedRules mk [lineCommentSaying d "[allowlist] the hosts we permit" cf]
          `shouldBe` ([], [])

  describe "parseAnnotation" $ do
    -- [allow:TestNoDummyValues] The bare marker is the case under test.
    it "rejects a bare marker with no rule" $
      forAllValid $ \(mk, sp, d) ->
        parsedWith mk sp (AttachmentToDecl d) CommentStyleLine "[allow] because"
          `shouldBe` Left AnnotationErrorNoRuleNamed
    -- [allow:TestNoDummyValues] A rule id no rule has.
    it "rejects an unknown rule" $
      forAllValid $ \(mk, sp, d) ->
        parsedWith mk sp (AttachmentToDecl d) CommentStyleLine "[allow:NoSuchRule] because"
          `shouldBe` Left (AnnotationErrorUnknownRuleId "NoSuchRule")
    -- [allow:TestNoDummyValues] A suppression with its reason left out.
    it "rejects a suppression with no reason" $
      forAllValid $ \(mk, sp, d) ->
        parsedWith mk sp (AttachmentToDecl d) CommentStyleLine "[allow:CommentBareTodo]"
          `shouldBe` Left AnnotationErrorNoReason
    -- [allow:TestNoDummyValues] Two rules where one belongs.
    it "rejects a comma list of rules" $
      forAllValid $ \(mk, sp, d) ->
        parsedWith mk sp (AttachmentToDecl d) CommentStyleLine "[allow:CommentBareTodo,HsGenValidInGenPackage] because"
          `shouldBe` Left AnnotationErrorTwoRulesAtOneSite
    -- [allow:TestNoDummyValues] A suppression that would parse anywhere it is allowed.
    it "refuses to sit in a documentation comment" $
      forAllValid $ \(mk, sp, d) ->
        parsedWith mk sp (AttachmentToDecl d) CommentStyleHaddockNext "[allow:CommentBareTodo] because"
          `shouldBe` Left AnnotationErrorInHaddock
    -- [allow:TestNoDummyValues] A suppression that would parse anywhere it is allowed.
    it "refuses to be unattached" $
      forAllValid $ \(mk, sp) ->
        parsedWith mk sp AttachmentToNothing CommentStyleLine "[allow:CommentBareTodo] because"
          `shouldBe` Left AnnotationErrorAttachedToNothing
    -- [allow:TestNoDummyValues] The marker and the rule, which the reason follows.
    it "accepts a rule and a reason" $
      forAllValid $ \(mk, sp, d) ->
        forAll (genValid `suchThat` isPlainReason) $ \reason ->
          parsedWith mk sp (AttachmentToDecl d) CommentStyleLine (T.concat ["[allow:CommentBareTodo] ", nonEmptyTextText reason])
            `shouldBe` Right
              AnnotationFact
                { annotationFactRule = RuleId "CommentBareTodo",
                  annotationFactScope = ScopeKeyOfDecl mk d,
                  annotationFactPrecision = AnnotationPrecisionDecl,
                  annotationFactReason = ReasonGiven reason,
                  annotationFactSpan = sp
                }
    -- [allow:TestNoDummyValues] The marker and the rule, which the reason follows.
    it "records the adoption reason as its own case" $
      forAllValid $ \(mk, sp, d) ->
        parsedWith mk sp (AttachmentToDecl d) CommentStyleLine (T.concat ["[allow:CommentBareTodo] ", adoptionReasonText])
          `shouldBe` Right
            AnnotationFact
              { annotationFactRule = RuleId "CommentBareTodo",
                annotationFactScope = ScopeKeyOfDecl mk d,
                annotationFactPrecision = AnnotationPrecisionDecl,
                annotationFactReason = ReasonAdoption,
                annotationFactSpan = sp
              }

  -- A rule that is never run over a file reports nothing in it, so every
  -- suppression written in one answers for nothing. The verdict is the same one
  -- 'applySuppression' reaches, and these assert that it is reached by the same
  -- steps: what counts as a suppression, and what counts as naming a rule.
  describe "unreadSuppressionsIn" $ do
    -- [allow:TestNoDummyValues] A file holding one suppression, on a line of its own.
    it "reports a suppression naming a rule as unused, from its marker to the end of the line" $
      forAllValid $ \file ->
        unreadSuppressionsIn shippedRules file "module Thing where\n\n-- [allow:CommentBareTodo] because\n"
          `shouldBe` ( [ Unused
                           { unusedRule = RuleId "CommentBareTodo",
                             unusedSpan =
                               Span
                                 { spanFile = file,
                                   spanStart =
                                     Position
                                       { positionLine = 3,
                                         positionCol = 4
                                       },
                                   spanEnd =
                                     Position
                                       { positionLine = 3,
                                         positionCol = 35
                                       }
                                 }
                           }
                       ],
                       []
                     )
    -- [allow:TestNoDummyValues] A file holding two suppressions with code between them.
    it "reports each of two suppressions rather than the file holding them" $
      forAllValid $ \file ->
        map (positionLine . spanStart . unusedSpan) (fst (unreadSuppressionsIn shippedRules file "-- [allow:CommentBareTodo] one\nx = 1\n-- [allow:HsNoFilePath] two\n"))
          `shouldBe` [1, 3]
    -- [allow:TestNoDummyValues] The bare marker is the case under test.
    it "reports one that names no rule as broken, exactly as a file that is read would" $
      forAllValid $ \file ->
        map annotationProblemMessage (snd (unreadSuppressionsIn shippedRules file "-- [allow] because\n"))
          `shouldBe` [renderAnnotationError AnnotationErrorNoRuleNamed]
    -- [allow:TestNoDummyValues] A rule id no rule has.
    it "reports one that names a rule nothing answers to as broken" $
      forAllValid $ \file ->
        map annotationProblemMessage (snd (unreadSuppressionsIn shippedRules file "-- [allow:NoSuchRule] because\n"))
          `shouldBe` [renderAnnotationError (AnnotationErrorUnknownRuleId "NoSuchRule")]
    -- [allow:TestNoDummyValues] A word that starts like the marker is the case under test.
    it "leaves a file that merely mentions a word starting the same way alone" $
      forAllValid $ \file ->
        unreadSuppressionsIn shippedRules file "module Thing where\n\n-- the [allowlist] of hosts we permit\n"
          `shouldBe` ([], [])
    -- [allow:TestNoDummyValues] A word that starts like the marker, and then a suppression.
    it "passes over such a word to the suppression written after it" $
      forAllValid $ \file ->
        map (positionCol . spanStart . unusedSpan) (fst (unreadSuppressionsIn shippedRules file "-- the [allowlist], and [allow:CommentBareTodo] because\n"))
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
      forAll (genValid `suchThat` (not . isWholeFileSpan . findingSpan)) $ \f ->
        forAllValid $ \(mk, d, reason) ->
          suppressionFor f {findingScope = ScopeKeyOfDecl mk d} reason
            `shouldBe` T.concat ["-- [allow:", unRuleId (findingRule f), "] ", reason]
    it "is file-scoped for a finding about a whole file" $
      forAllValid $ \(f, file, reason) ->
        suppressionFor f {findingSpan = wholeFileSpan file} reason
          `shouldBe` T.concat ["-- [allow:file:", unRuleId (findingRule f), "] ", reason]
    it "is file-scoped for a finding whose comment attaches to no declaration" $
      forAllValid $ \(f, mk, reason) ->
        suppressionFor f {findingScope = ScopeKeyOfFile mk} reason
          `shouldBe` T.concat ["-- [allow:file:", unRuleId (findingRule f), "] ", reason]

  describe "applySuppression" $ do
    it "leaves an unannotated finding alone" $
      forAllValid $ \f ->
        applySuppression [] [f]
          `shouldBe` Suppression
            { suppressionRemaining = [f],
              suppressionUnused = [],
              suppressionOverBroad = []
            }
    it "reports an annotation that suppresses nothing" $
      forAllValid $ \a ->
        applySuppression [a] []
          `shouldBe` Suppression
            { suppressionRemaining = [],
              suppressionUnused =
                [ Unused
                    { unusedRule = annotationFactRule a,
                      unusedSpan = annotationFactSpan a
                    }
                ],
              suppressionOverBroad = []
            }
    it "reports an annotation that suppresses more than one finding" $
      forAllValid $ \(a, f, g) ->
        applySuppression [a] [answeredBy a f, answeredBy a g]
          `shouldBe` Suppression
            { suppressionRemaining = [],
              suppressionUnused = [],
              suppressionOverBroad =
                [ OverBroad
                    { overBroadAnnotation = a,
                      overBroadCount = 2
                    }
                ]
            }

    -- Two suppressions and two findings on four lines in a row, each suppression
    -- nearest one finding. Both answer for something, so neither is unused and
    -- neither is over-broad.
    --
    -- The nearest one has to win for that to come out, and a line number is a
    -- 'Word': the distance cannot be a subtraction, because one taken from the
    -- other is not a small negative but a number larger than any file.
    it "pairs each suppression with the finding nearest it" $
      forAllValid $ \(a, f, g) ->
        forAll (genValid `suchThat` (< maxBound - 3)) $ \top ->
          case [top ..] of
            (aboveLine : firstLine : secondLine : belowLine : _) ->
              let declared = a {annotationFactPrecision = AnnotationPrecisionDecl}
                  above = declared {annotationFactSpan = onLine aboveLine (annotationFactSpan a)}
                  below = declared {annotationFactSpan = onLine belowLine (annotationFactSpan a)}
                  first = (answeredBy declared f) {findingSpan = onLine firstLine (findingSpan f)}
                  second = (answeredBy declared g) {findingSpan = onLine secondLine (findingSpan g)}
               in applySuppression [above, below] [first, second]
                    `shouldBe` Suppression
                      { suppressionRemaining = [],
                        suppressionUnused = [],
                        suppressionOverBroad = []
                      }
            _ -> expectationFailure "Four lines in a row were asked for where there are not four."

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
            (ruleSet builtinRules [ruleId BareTodo.rule])
        eModels <- discoverPackages root (sourceRootDir root)
        case eModels of
          Left err -> expectationFailure (T.unpack (renderDiscoveryError err))
          Right models -> do
            named <- forM models $ \pm -> do
              let name = unPackageName (packageModelName pm)
              dir <- (tmp </>) <$> parseRelDir (T.unpack name)
              ensureDir dir
              report <- runPackageCommand withEverything noHieDirectories root [] (packageModelDir pm) (Just dir)
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
            [t | ComplaintFailure t <- unComplaints report]
              `shouldBe` replicate
                3
                (FailureFactsIncomplete (StoreProblemSuppressionNamesRuleNotRun (ruleId BareTodo.rule)))

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
