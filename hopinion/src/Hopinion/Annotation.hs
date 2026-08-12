{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Suppression annotations: parsing them, matching them against findings, and
-- reporting the ones that suppress nothing.
--
-- The keystone is that an annotation suppressing nothing is an error. That is
-- what makes on-by-default plus unlimited local escapes safe: suppressions
-- cannot accumulate silently.
module Hopinion.Annotation
  ( annotationsOf,
    AnnotationError (..),
    renderAnnotationError,
    parseAnnotation,
    suppresses,
    OverBroad (..),
    Suppression (..),
    applySuppression,
    suppressionFor,
    suppressionIsFileScoped,
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import GHC.Generics (Generic)
import Hopinion.Comment
import Hopinion.Facts
import Hopinion.Rule
import Hopinion.Rule.Id

-- | Every comment that starts with the marker, whether it parses or not. A
-- comment that meant to be a suppression and failed is reported rather than
-- ignored.
annotationsOf :: RuleSet -> ModuleRef -> [CommentFact] -> ([AnnotationFact], [AnnotationProblem])
annotationsOf rs mk = foldr step ([], [])
  where
    step cf (facts, problems)
      | not (meansToBeOne (T.stripStart (commentFactText cf))) = (facts, problems)
      | otherwise = case parseAnnotation rs mk cf of
          Right f -> (f : facts, problems)
          Left err -> (facts, AnnotationProblem {annotationProblemSpan = commentFactSpan cf, annotationProblemMessage = renderAnnotationError err} : problems)

-- | Whether a comment means to be a suppression, which is what decides between
-- reporting it as a broken one and leaving it alone.
--
-- The marker has to be closed or followed by a colon. On the prefix alone,
-- @[allowlist]@ and @[allowance]@ are comments a run would fail over, and a
-- comment nobody meant as a suppression is the one thing this must not refuse.
--
-- A bare @[allow]@ does mean to be one, and is refused further in for naming no
-- rule.
meansToBeOne :: Text -> Bool
meansToBeOne t = any (`T.isPrefixOf` t) [T.snoc marker ']', T.snoc marker ':']

marker :: Text
marker = "[allow"

-- | Every way a comment that means to be a suppression fails to be one.
--
-- Typed because each is a different fix, and because the parser and the report
-- would otherwise agree on the wording by copying it. 'NotASuppression' is the
-- one that is not a mistake: it is what every comment that is not one answers,
-- and 'annotationsOf' reads it as "nothing to see here" rather than as a
-- problem.
data AnnotationError
  = NotASuppression
  | InHaddock
  | NoClosingBracket
  | NoRuleNamed
  | TwoRulesAtOneSite
  | UnknownRuleId !Text
  | RuleIsTurnedOff !RuleId
  | NoReason
  | AttachedToNothing
  deriving (Show, Eq)

renderAnnotationError :: AnnotationError -> Text
renderAnnotationError = \case
  NotASuppression -> "not a suppression"
  InHaddock -> "A suppression has no business in generated documentation."
  NoClosingBracket -> "A suppression needs a closing bracket."
  NoRuleNamed -> "A bare [allow] would silently absorb rules added later. Name the rule."
  TwoRulesAtOneSite -> "One rule per annotation: two rules at one site means two reasons."
  UnknownRuleId t -> T.concat ["Unknown rule id: ", t]
  RuleIsTurnedOff rid ->
    T.concat
      [ "This suppresses ",
        ruleIdText rid,
        ", which this repository has turned off, so it suppresses nothing. Remove it."
      ]
  NoReason -> "A suppression with no reason is a config exception with extra steps."
  AttachedToNothing -> "This suppression is not attached to anything. Move it against the code it concerns."

parseAnnotation :: RuleSet -> ModuleRef -> CommentFact -> Either AnnotationError AnnotationFact
parseAnnotation rs mk cf = do
  assertNotHaddock
  (fileScoped, ruleText', reason) <- splitAnnotation (T.stripStart (commentFactText cf))
  ruleId' <- namedRule ruleText'
  reason' <-
    maybe
      (Left NoReason)
      Right
      (nonEmptyText (T.strip reason))
  (scope, precision) <- placement fileScoped
  pure
    AnnotationFact
      { annotationFactRule = ruleId',
        annotationFactScope = scope,
        annotationFactPrecision = precision,
        annotationFactReason =
          if nonEmptyTextText reason' == adoptionReasonText then ReasonAdoption else ReasonGiven reason',
        annotationFactSpan = commentFactSpan cf
      }
  where
    assertNotHaddock =
      if commentFactStyle cf `elem` [StyleHaddockNext, StyleHaddockPrev, StyleHaddockNamed]
        then Left InHaddock
        else Right ()

    -- Three answers rather than two, since the rule set is a repository's to
    -- choose. A name nothing answers to is a typo, and the rule the writer meant
    -- is still running. A name this run knows and does not run is a suppression
    -- left behind by turning that rule off, reported for the same reason one
    -- that has outlived its finding is: it answers for nothing.
    namedRule t = do
      rid <- maybe (Left (UnknownRuleId t)) Right (parseRuleId t)
      case useOf rs rid of
        Just RuleRuns -> Right rid
        Nothing -> Left (UnknownRuleId t)
        Just RuleTurnedOff -> Left (RuleIsTurnedOff rid)

    placement fileScoped = case (fileScoped, commentFactAttachment cf) of
      (FileScoped, _) -> Right (ScopeOfFile mk, PrecisionFile)
      (SiteScoped, AttachedToDecl d) -> Right (ScopeOfDecl mk d, PrecisionDecl)
      (SiteScoped, AttachedToStatement d sp) -> Right (ScopeOfDecl mk d, PrecisionStatement sp)
      (SiteScoped, _) ->
        Left AttachedToNothing

data AnnotationReach
  = FileScoped
  | SiteScoped
  deriving stock (Show, Eq, Generic)

instance Validity AnnotationReach

-- | @[allow:RuleId] reason@, or @[allow:file:RuleId] reason@. One rule per
-- annotation and no comma lists, because two rules at one site means two
-- reasons.
splitAnnotation :: Text -> Either AnnotationError (AnnotationReach, Text, Text)
splitAnnotation t = do
  afterMarker <- maybe (Left NotASuppression) Right (T.stripPrefix marker t)
  (inside, reason) <- case T.breakOn "]" afterMarker of
    (_, rest) | T.null rest -> Left NoClosingBracket
    (inside, rest) -> Right (inside, T.drop 1 rest)
  body <- maybe (Left NoRuleNamed) Right (T.stripPrefix ":" inside)
  case T.stripPrefix "file:" body of
    Just rest -> pure (FileScoped, T.strip rest, reason)
    Nothing ->
      if T.isInfixOf "," body
        then Left TwoRulesAtOneSite
        else pure (SiteScoped, T.strip body, reason)

-- | An annotation suppresses a finding when the rule ids match and the
-- annotation reaches the finding's site.
suppresses :: AnnotationFact -> Finding -> Bool
suppresses a f =
  annotationFactRule a == findingRule f
    && scopeKeyModule (annotationFactScope a) == scopeKeyModule (findingScope f)
    && case annotationFactPrecision a of
      PrecisionFile -> True
      PrecisionDecl -> annotationFactScope a == findingScope f
      PrecisionStatement sp -> spanContains sp (findingSpan f)

-- | A suppression answering for more than one finding, and for how many.
--
-- A record rather than a pair, so that neither end can be read for the other
-- and the count has a name saying what it counts.
data OverBroad = OverBroad
  { overBroadAnnotation :: !AnnotationFact,
    overBroadCount :: !Word
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec OverBroad)

instance Validity OverBroad

instance HasCodec OverBroad where
  codec =
    object "OverBroad" $
      OverBroad
        <$> requiredField "annotation" "the suppression" .= overBroadAnnotation
        <*> requiredField "count" "how many findings it answers for" .= overBroadCount

data Suppression = Suppression
  { suppressionRemaining :: ![Finding],
    -- | Reported and failing, exactly like a violation.
    suppressionUnused :: ![AnnotationFact],
    -- | A coarse annotation absorbing several findings is not an error, but it
    -- is the mechanism by which annotations quietly swallow new violations, so
    -- it is reported.
    suppressionOverBroad :: ![OverBroad]
  }
  deriving stock (Show, Eq, Generic)

instance Validity Suppression

-- | The annotations passed in must be exactly those naming rules this level
-- judges, so that no annotation falls between two levels.
--
-- Exactly one annotation answers for each finding. Asking instead whether an
-- annotation covers any finding at all would let a second annotation over the
-- same code ride along on the first one's back forever.
applySuppression :: [AnnotationFact] -> [Finding] -> Suppression
applySuppression annotations findings =
  Suppression
    { suppressionRemaining = [f | (f, Nothing) <- answered],
      suppressionUnused = [a | (i, a) <- indexed, answersFor i == 0],
      suppressionOverBroad =
        [ OverBroad {overBroadAnnotation = a, overBroadCount = answersFor i}
        | (i, a) <- indexed,
          answersFor i > 1
        ]
    }
  where
    indexed :: [(Word, AnnotationFact)]
    indexed = zip [0 ..] annotations

    responsibleFor :: Finding -> Maybe Word
    responsibleFor f = case sortOn (rank f) [ia | ia <- indexed, suppresses (snd ia) f] of
      ((i, _) : _) -> Just i
      [] -> Nothing

    -- Which annotation answers for each finding, worked out once: both of the
    -- other answers are counts of this.
    answered :: [(Finding, Maybe Word)]
    answered = [(f, responsibleFor f) | f <- findings]

    counts :: M.Map Word Word
    counts = M.fromListWith (+) [(i, 1 :: Word) | (_, Just i) <- answered]

    answersFor :: Word -> Word
    answersFor i = M.findWithDefault 0 i counts

    -- The most precisely placed annotation answers for a finding, then the
    -- nearest, then the earlier one. Any total order would make the choice
    -- deterministic; this one makes it the annotation a reader would say the
    -- finding belongs to.
    rank :: Finding -> (Word, AnnotationFact) -> (Word, Word, Position)
    rank f (_, a) =
      ( specificity (annotationFactPrecision a),
        linesApart (spanStart (annotationFactSpan a)) (spanStart (findingSpan f)),
        spanStart (annotationFactSpan a)
      )

    -- Not @abs (a - b)@: these are 'Word's, and the annotation is above the
    -- finding in every suppression anybody writes, so that subtraction is the
    -- one that goes below zero and comes back as a number larger than any file.
    -- Every annotation above its finding would rank as the furthest away.
    linesApart :: Position -> Position -> Word
    linesApart a b =
      let x = positionLine a
          y = positionLine b
       in if x > y then x - y else y - x

    specificity :: AnnotationPrecision -> Word
    specificity p = case p of
      PrecisionStatement _ -> 0
      PrecisionDecl -> 1
      PrecisionFile -> 2

-- | The suppression that answers for this finding, which is the same text
-- whether it is written into a file or offered to a reader. The reason is left
-- to the caller to finish.
--
-- Two forms, and the finding decides which. A site-scoped comment only attaches
-- to a declaration or to a statement, so a finding about neither needs the
-- file-scoped form or the next run would reject the suppression as attached to
-- nothing.
--
-- Both halves of the test are load-bearing and neither implies the other: a
-- comment before the module header has a real span and a file scope, and a
-- generated instance has a declaration scope and no line to point at.
suppressionFor :: Finding -> Text -> Text
suppressionFor f reason =
  T.concat
    [ "-- [allow:",
      if suppressionIsFileScoped f then "file:" else "",
      ruleIdText (findingRule f),
      "] ",
      reason
    ]

-- | Whether the suppression for this finding has to reach the whole file.
--
-- Exported so the hint a reader is given and the text it offers cannot disagree
-- about which form it is.
suppressionIsFileScoped :: Finding -> Bool
suppressionIsFileScoped f =
  isWholeFileSpan (findingSpan f) || isFileScope (findingScope f)
  where
    isFileScope = \case
      ScopeOfFile _ -> True
      ScopeOfDecl _ _ -> False
