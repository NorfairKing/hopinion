{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The output contract: what a run has to say, how it crosses a process
-- boundary, and where it sits on disk.
--
-- Showing any of it to a person needs the code it is about, which the project
-- layer does not have. So the drawing of a report lives in
-- "Hopinion.Report.Render" and this module knows nothing about it: a command
-- that only has to judge a report must not need a source tree.
module Hopinion.Report
  ( ParseFailure (..),
    Failure (..),
    StoreProblem (..),
    renderStoreProblem,
    renderFailure,
    Complaint (..),
    Complaints (..),
    complaintsOf,
    complaintsFindings,
    failureComplaints,
    isClean,
    encodeReport,
    ReportError (..),
    ReportDirError (..),
    renderReportDirError,
    decodeReport,
    factsFile,
    reportDataFile,
    reportTextFile,
    readReportFrom,
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Validity
import Data.Validity.Containers ()
import GHC.Generics (Generic)
import Hopinion.Annotation (OverBroad (..), Unused (..))
import Hopinion.Facts.Name
import Hopinion.Facts.Place
import Hopinion.Facts.Suppression
import Hopinion.Rule
import Hopinion.Rule.Id
import Path (Abs, Dir, File, Path, Rel, relfile, toFilePath, (</>))

-- | Where a module stopped being readable, and what the parser said about it.
-- The message is GHC's own, passed through rather than owned here.
data ParseFailure = ParseFailure
  { parseFailurePath :: !(Path Rel File),
    parseFailurePosition :: !Position,
    parseFailureMessage :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ParseFailure)

instance Validity ParseFailure

instance HasCodec ParseFailure where
  codec =
    object "ParseFailure" $
      ParseFailure
        <$> requiredFieldWith "path" relPathCodec "the module that did not parse" .= parseFailurePath
        <*> requiredField "position" "where it stopped" .= parseFailurePosition
        <*> requiredField "message" "what the parser said" .= parseFailureMessage

-- | What stopped the tool from being able to tell, as what happened rather than
-- as the sentence about it.
--
-- Typed, because a reader of a report is not always a person: this crosses to
-- disk, and a caller deciding what a failure means should not have to match on
-- prose. 'renderFailure' is the only place any of these is put into words.
--
-- What a failure carries is a path relative to the repository, a typed
-- 'StoreProblem', or nothing at all. The ones carrying a rendered sentence say
-- at each constructor why: what they are about is an absolute path, and a store
-- path in a report is somewhere nobody reading it can navigate to.
data Failure
  = FailureModuleDidNotParse !ParseFailure
  | FailureNoSourceFor !(Path Rel File)
  | FailureRepositoryUnreadable !Text
  | FailureChoicesRefused !Text
  | FailureRuleSetRefused !Text
  | -- | A package output this run was given and cannot use: absent, repeated,
    -- written by another build of the tool, or holding facts that will not
    -- merge.
    --
    -- Rendered text, like the three above, because what it is about is an
    -- absolute path. A run under Nix is handed store paths, and a store path in
    -- a report is somewhere nobody reading it can navigate to, so there is
    -- nothing for a consumer to do with it but print the sentence.
    FailurePackageOutputRefused !Text
  | -- | A build was given @.hie@ files and could not answer for a module it was
    -- held to covering. Rendered for the reason above: it names the directories
    -- it looked in.
    FailureArtifactsRefused !Text
  | -- | The project layer was given no package to expect, which is the one way
    -- it could answer "clean" without having read anything.
    FailureNoPackagesExpected
  | FailureFactsIncomplete !StoreProblem
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Failure)

instance Validity Failure

-- | What the facts a project run merged do not add up to.
--
-- Every one of these would otherwise be an obligation quietly satisfied: a
-- package whose facts never arrived, a module the cabal file declares and
-- nothing could read, a suppression naming a rule this run does not make, or a
-- module a build was supposed to have compiled and did not. All of them are
-- about names rather than about paths on the machine, so all of them are typed.
data StoreProblem
  = StoreProblemNoFactsForPackage !PackageName
  | StoreProblemFactsFromAnotherVersion
  | StoreProblemPackageDoesNotCover !PackageName !ModuleRef
  | StoreProblemModuleDidNotParse !ModuleKey
  | StoreProblemSuppressionNamesRuleNotRun !RuleId
  | StoreProblemNoArtifactFor !(Path Rel File)
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec StoreProblem)

instance Validity StoreProblem

instance HasCodec StoreProblem where
  codec =
    named "StoreProblem" $
      object "StoreProblem" $
        discriminatedUnionCodec "kind" enc dec
    where
      enc = \case
        StoreProblemNoFactsForPackage p -> ("no-facts", mapToEncoder p (requiredField' "package"))
        StoreProblemFactsFromAnotherVersion -> ("another-version", mapToEncoder () (pureCodec ()))
        StoreProblemPackageDoesNotCover p ref -> ("module-not-covered", mapToEncoder (p, ref) notCovered)
        StoreProblemModuleDidNotParse mk -> ("did-not-parse", mapToEncoder mk (requiredField' "module"))
        StoreProblemSuppressionNamesRuleNotRun rid -> ("rule-not-run", mapToEncoder rid (requiredField' "rule"))
        StoreProblemNoArtifactFor rp -> ("no-artifact", mapToEncoder rp (requiredFieldWith' "path" relPathCodec))
      dec =
        HM.fromList
          [ ("no-facts", ("a package whose facts never arrived", mapToDecoder StoreProblemNoFactsForPackage (requiredField' "package"))),
            ("another-version", ("facts written by another build of the tool", mapToDecoder (const StoreProblemFactsFromAnotherVersion) (pureCodec ()))),
            ( "module-not-covered",
              ( "a module a package declares and its facts do not cover",
                mapToDecoder (uncurry StoreProblemPackageDoesNotCover) notCovered
              )
            ),
            ("did-not-parse", ("a module the tool could not read", mapToDecoder StoreProblemModuleDidNotParse (requiredField' "module"))),
            ("rule-not-run", ("a suppression naming a rule this run does not make", mapToDecoder StoreProblemSuppressionNamesRuleNotRun (requiredField' "rule"))),
            ("no-artifact", ("a module a build that was given did not cover", mapToDecoder StoreProblemNoArtifactFor (requiredFieldWith' "path" relPathCodec)))
          ]

      notCovered :: JSONObjectCodec (PackageName, ModuleRef)
      notCovered = (,) <$> requiredField' "package" .= fst <*> requiredField' "module" .= snd

renderStoreProblem :: StoreProblem -> Text
renderStoreProblem = \case
  StoreProblemNoFactsForPackage p -> T.concat ["No facts for expected package ", packageNameText p]
  StoreProblemFactsFromAnotherVersion ->
    "The fact store was written in a different format than this build of the tool reads."
  StoreProblemPackageDoesNotCover p ref ->
    T.concat
      [ "Package ",
        packageNameText p,
        " declares module ",
        moduleKeyText (moduleRefModule ref),
        " in ",
        componentNameText (moduleRefComponent ref),
        " but its facts do not cover it"
      ]
  StoreProblemModuleDidNotParse mk -> T.concat ["Module ", moduleKeyText mk, " failed to parse"]
  StoreProblemSuppressionNamesRuleNotRun rid ->
    T.concat
      [ "A suppression in the facts names ",
        ruleIdText rid,
        ", which this run does not run. ",
        "The package outputs and this run were given different rule sets."
      ]
  StoreProblemNoArtifactFor rp ->
    T.concat
      [ "No .hie file for ",
        relPathText rp,
        ", which a build that was given must cover. ",
        "Test code is not compiled unless a package set is told to, ",
        "and two components whose modules are both called Main need a directory each."
      ]

instance HasCodec Failure where
  codec =
    named "Failure" $
      object "Failure" $
        discriminatedUnionCodec "kind" enc dec
    where
      enc = \case
        FailureModuleDidNotParse pf -> ("did-not-parse", mapToEncoder pf (requiredField' "did-not-parse"))
        FailureNoSourceFor rp -> ("no-source", mapToEncoder rp (requiredFieldWith' "no-source" relPathCodec))
        FailureRepositoryUnreadable t -> ("unreadable", mapToEncoder t (requiredField' "unreadable"))
        FailureChoicesRefused t -> ("choices-refused", mapToEncoder t (requiredField' "choices-refused"))
        FailureRuleSetRefused t -> ("rules-refused", mapToEncoder t (requiredField' "rules-refused"))
        FailurePackageOutputRefused t -> ("output-refused", mapToEncoder t (requiredField' "output-refused"))
        FailureArtifactsRefused t -> ("artifacts-refused", mapToEncoder t (requiredField' "artifacts-refused"))
        FailureNoPackagesExpected -> ("no-packages", mapToEncoder () (pureCodec ()))
        FailureFactsIncomplete p -> ("facts-incomplete", mapToEncoder p (requiredField' "facts-incomplete"))
      dec =
        HM.fromList
          [ ("did-not-parse", ("a module the tool could not read", mapToDecoder FailureModuleDidNotParse (requiredField' "did-not-parse"))),
            ("no-source", ("a reported path no source root accounts for", mapToDecoder FailureNoSourceFor (requiredFieldWith' "no-source" relPathCodec))),
            ("unreadable", ("what stopped the repository from being read", mapToDecoder FailureRepositoryUnreadable (requiredField' "unreadable"))),
            ("choices-refused", ("what this repository asked for that is refused", mapToDecoder FailureChoicesRefused (requiredField' "choices-refused"))),
            ("rules-refused", ("why the rules this run was asked for are not a set", mapToDecoder FailureRuleSetRefused (requiredField' "rules-refused"))),
            ("output-refused", ("a package output this run cannot use", mapToDecoder FailurePackageOutputRefused (requiredField' "output-refused"))),
            ("artifacts-refused", ("what a build did not answer for", mapToDecoder FailureArtifactsRefused (requiredField' "artifacts-refused"))),
            ("no-packages", ("the project layer was given nothing to expect", mapToDecoder (const FailureNoPackagesExpected) (pureCodec ()))),
            ("facts-incomplete", ("what the merged facts do not add up to", mapToDecoder FailureFactsIncomplete (requiredField' "facts-incomplete")))
          ]

renderFailure :: Failure -> Text
renderFailure = \case
  FailureModuleDidNotParse pf ->
    T.concat
      [ relPathText (parseFailurePath pf),
        ":",
        T.pack (show (positionLine (parseFailurePosition pf))),
        ":",
        T.pack (show (positionCol (parseFailurePosition pf))),
        ": ",
        parseFailureMessage pf
      ]
  FailureNoSourceFor rp ->
    T.concat
      [ "No source root accounts for ",
        relPathText rp,
        ", so it cannot be shown against its code. Pass --source PREFIX=DIR for the package it is in."
      ]
  FailureRepositoryUnreadable t -> t
  FailureChoicesRefused t -> t
  FailureRuleSetRefused t -> t
  FailurePackageOutputRefused t -> t
  FailureArtifactsRefused t -> t
  FailureNoPackagesExpected -> "The project layer was given no packages to expect."
  FailureFactsIncomplete p -> renderStoreProblem p

-- | One thing a run has to complain about.
--
-- One list rather than a list per kind, because every one of them fails the run
-- and only the rendering treats them differently. A list apiece would be an
-- append apiece in the 'Semigroup', an empty apiece in the 'Monoid', a field
-- apiece in the codec, and a way to add a kind and forget one of them.
--
-- A reader must still be able to tell "the code is wrong" from "the tool could
-- not tell", which is what 'ComplaintFailure' is: it carries no position,
-- because there is no code to point at.
data Complaint
  = ComplaintFinding !Finding
  | ComplaintUnused !Unused
  | ComplaintOverBroad !OverBroad
  | ComplaintProblem !AnnotationProblem
  | ComplaintFailure !Failure
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Complaint)

instance Validity Complaint

instance HasCodec Complaint where
  codec =
    named "Complaint" $
      object "Complaint" $
        discriminatedUnionCodec "kind" enc dec
    where
      enc = \case
        ComplaintFinding f -> ("finding", mapToEncoder f (requiredField' "finding"))
        ComplaintUnused a -> ("unused", mapToEncoder a (requiredField' "unused"))
        ComplaintOverBroad o -> ("over-broad", mapToEncoder o (requiredField' "over-broad"))
        ComplaintProblem p -> ("problem", mapToEncoder p (requiredField' "problem"))
        ComplaintFailure t -> ("failure", mapToEncoder t (requiredField' "failure"))
      dec =
        HM.fromList
          [ ("finding", ("code a rule reported on", mapToDecoder ComplaintFinding (requiredField' "finding"))),
            ("unused", ("a suppression that answers for nothing", mapToDecoder ComplaintUnused (requiredField' "unused"))),
            ("over-broad", ("a suppression answering for more than one finding", mapToDecoder ComplaintOverBroad (requiredField' "over-broad"))),
            ("problem", ("a comment that meant to be a suppression", mapToDecoder ComplaintProblem (requiredField' "problem"))),
            ("failure", ("what stopped the tool from being able to tell", mapToDecoder ComplaintFailure (requiredField' "failure")))
          ]

-- | Everything a run has to say, which is everything it has to complain about.
-- A clean run is 'mempty', which is the whole of what 'isClean' has to ask.
newtype Complaints = Complaints {complaintsList :: [Complaint]}
  deriving stock (Show, Eq, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving (FromJSON, ToJSON) via (Autodocodec Complaints)

instance Validity Complaints

instance HasCodec Complaints where
  codec = named "Complaints" (dimapCodec Complaints complaintsList codec)

complaintsFindings :: Complaints -> [Finding]
complaintsFindings cs = [f | ComplaintFinding f <- complaintsList cs]

-- | The failures alone, which is what a caller asking "could the tool tell at
-- all" wants, and what a test asserting the answer to that asks.
complaintsOf :: [Complaint] -> Complaints
complaintsOf = Complaints

-- | Nothing but what the tool could not do. Plural because each reason is a fix,
-- and hearing them one run at a time is one build apiece.
failureComplaints :: [Failure] -> Complaints
failureComplaints = complaintsOf . map ComplaintFailure

-- | The only question a caller deciding whether to fail has to ask. A run with
-- nothing to complain about is one whose complaints are 'mempty', so this
-- cannot go stale when a kind of complaint is added.
isClean :: Complaints -> Bool
isClean = (== mempty)

-- | Reports cross a process boundary the same way facts do, and for the same
-- reason: the layer that produces one is not the layer that decides what it
-- means.
encodeReport :: Complaints -> LB.ByteString
encodeReport = JSON.encode . toJSONViaCodec

-- | What can stop a report on disk from being read back.
--
-- Two constructors, because they are different fixes: bytes that are not JSON
-- are a truncated or clobbered file, and JSON that is not a report was written
-- by a hopinion whose format is not this one. Both messages are aeson's own,
-- passed through rather than owned here.
data ReportError
  = ReportErrorNotJson !Text
  | ReportErrorNotAReport !Text
  deriving stock (Show, Eq)

-- | One of those and the directory it was read from, which is what tells a
-- reader which package's output to look at.
data ReportDirError = ReportDirError !(Path Abs Dir) !ReportError
  deriving stock (Show, Eq)

renderReportDirError :: ReportDirError -> Text
renderReportDirError (ReportDirError dir err) =
  T.pack (unwords [concat [toFilePath dir, ":"], T.unpack (said err)])
  where
    said = \case
      ReportErrorNotJson t -> t
      ReportErrorNotAReport t -> t

decodeReport :: LB.ByteString -> Either ReportError Complaints
decodeReport bs = case JSON.eitherDecode bs of
  Left err -> Left (ReportErrorNotJson (T.pack err))
  Right value -> case JSON.parseEither parseJSONViaCodec value of
    Left err -> Left (ReportErrorNotAReport (T.pack err))
    Right report -> Right report

-- | What a package's output directory holds: the facts to read, the report as
-- data to judge, and the report as text for a person. Named together because a
-- directory with only some of them is one the next command cannot use.
factsFile :: Path Rel File
factsFile = [relfile|facts.db|]

reportDataFile :: Path Rel File
reportDataFile = [relfile|report.json|]

reportTextFile :: Path Rel File
reportTextFile = [relfile|report.txt|]

readReportFrom :: Path Abs Dir -> IO (Either ReportDirError (Complaints, Text))
readReportFrom dir = do
  contents <- LB.readFile (toFilePath (dir </> reportDataFile))
  case decodeReport contents of
    Left err -> pure (Left (ReportDirError dir err))
    Right report -> do
      rendered <- TE.decodeUtf8 <$> BS.readFile (toFilePath (dir </> reportTextFile))
      pure (Right (report, rendered))
