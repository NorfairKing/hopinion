{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What a rule is, and what running one produces.
--
-- Checks are values in a registry rather than instances of a class: no generic
-- code is polymorphic in a check type, so a record of functions is the right
-- encoding.
module Hopinion.Rule
  ( Finding (..),
    CheckResult (..),
    ModuleCheck (..),
    moduleCheckFindings,
    PackageCheck (..),
    ProjectCheck (..),
    Carry,
    Query,
    ruleLevel,
    ruleMigrations,
    carryOf,
    noResult,
    findingsResult,
    Rule (..),
    RuleImpl (..),
    RuleSet,
    RuleUse (..),
    RuleSetError (..),
    renderRuleSetError,
    ruleSet,
    emptyRuleSet,
    ruleSetRules,
    ruleSetTurnedOff,
    useOf,
    ruleFor,
    ruleNamed,
    rulesAtLevel,
    withoutRules,
    scopeOfComment,
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Database.Persist.Sql (Migration)
import GHC.Generics (Generic)
import Hopinion.Comment
import Hopinion.Compiled (CompiledModules)
import Hopinion.Facts
import Hopinion.Rule.Id
import Hopinion.Store (Carry, Query)
import Text.Colour (Chunk, chunk, fore, red)

data Finding = Finding
  { findingRule :: !RuleId,
    -- | Coarse, and portable across the fact boundary.
    findingScope :: !ScopeKey,
    -- | Precise, for the report and for statement-level annotation matching.
    findingSpan :: !Span,
    findingMessage :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Finding)

instance Validity Finding

instance HasCodec Finding where
  codec =
    object "Finding" $
      Finding
        <$> requiredField "rule" "the rule that produced it" .= findingRule
        <*> requiredField "scope" "what it is about, portably" .= findingScope
        <*> requiredField "span" "where it is" .= findingSpan
        <*> requiredField "message" "what is wrong" .= findingMessage

-- | What a rule has to say, which is what it found and nothing else.
--
-- There is no third answer. Every way a rule could fail to tell is either a
-- demand on the build, which fails the run, or a question narrowed until it is
-- answerable.
newtype CheckResult = CheckResult
  { checkResultFindings :: [Finding]
  }
  deriving stock (Show, Eq, Generic)

instance Semigroup CheckResult where
  a <> b =
    CheckResult
      { checkResultFindings = checkResultFindings a ++ checkResultFindings b
      }

instance Monoid CheckResult where
  mempty = noResult

noResult :: CheckResult
noResult = CheckResult {checkResultFindings = []}

findingsResult :: [Finding] -> CheckResult
findingsResult fs = CheckResult {checkResultFindings = fs}

-- | What a rule does, and where its findings come from.
--
-- A rule above the module level owns a table: the migration, what it writes out
-- of one module, and the query it answers with. The envelope never learns what
-- is in that table, which is why adding a rule adds no case here.
--
-- The constructor is also the level, so nothing can disagree with it, and it is
-- the scope of what a build had to say. The envelope hands the material over
-- and takes no view on it, so a rule decides for itself what generated code
-- means rather than being told by a fact computed on its behalf.
data RuleImpl
  = ModuleRule !ModuleCheck
  | PackageRule !PackageCheck
  | ProjectRule !ProjectCheck

-- | A rule that can only see one module at a time. It carries nothing, because
-- everything it reads has become a finding by the time the module phase
-- returns.
--
-- Pure, over the source alone. A module rule wanting what the compiler wrote
-- down would take it as a second argument; nothing does yet. Package and
-- project rules are unaffected, since those already run in IO.
newtype ModuleCheck = FromSource (ModuleContext -> CheckResult)

moduleCheckFindings :: ModuleCheck -> ModuleContext -> CheckResult
moduleCheckFindings (FromSource f) = f

data PackageCheck = PackageCheck
  { packageCheckMigration :: Migration,
    packageCheckCarry :: PackageName -> ModuleContext -> Carry,
    packageCheckFindings :: PackageName -> CompiledModules -> Query CheckResult
  }

data ProjectCheck = ProjectCheck
  { projectCheckMigration :: Migration,
    projectCheckCarry :: PackageName -> ModuleContext -> Carry,
    projectCheckFindings :: CompiledModules -> Query CheckResult
  }

ruleLevel :: Rule -> Level
ruleLevel r = case ruleImpl r of
  ModuleRule _ -> LevelModule
  PackageRule _ -> LevelPackage
  ProjectRule _ -> LevelProject

-- | Every table a rule brought, so that a store is created with room for all of
-- them whether or not anything writes one.
ruleMigrations :: [Rule] -> [Migration]
ruleMigrations rules =
  [ m
  | r <- rules,
    m <- case ruleImpl r of
      ModuleRule _ -> []
      PackageRule c -> [packageCheckMigration c]
      ProjectRule c -> [projectCheckMigration c]
  ]

-- | What this rule writes out of one module, or nothing when it writes nothing.
carryOf :: Rule -> PackageName -> ModuleContext -> Carry
carryOf r pkg ctx = case ruleImpl r of
  ModuleRule _ -> pure ()
  PackageRule c -> packageCheckCarry c pkg ctx
  ProjectRule c -> projectCheckCarry c pkg ctx

-- | Every rule owns its own metadata, so there is no central table of levels,
-- classes or guide references to edit when a rule is added.
data Rule = Rule
  { ruleId :: !RuleId,
    -- | What the rule asks for, in one sentence.
    ruleText :: !Text,
    -- | Why it asks: what goes wrong in code that does not, said well enough
    -- that a reader just stopped by it can choose between fixing the code and
    -- writing the suppression. A rule that cannot answer this is one nobody
    -- will believe.
    ruleWhy :: !Text,
    ruleImpl :: !RuleImpl
  }

-- | Whether a rule in a set is one this run makes.
--
-- Carried beside the rule rather than kept as a second list, because a rule
-- turned off is still one this run knows: a suppression naming it is a
-- suppression to remove, where one naming nothing is a typo.
data RuleUse
  = RuleRuns
  | RuleTurnedOff
  deriving stock (Show, Eq, Generic)

instance Validity RuleUse

-- | One rule, and whether this run makes it.
data RuleEntry = RuleEntry
  { entryRule :: !Rule,
    entryUse :: !RuleUse
  }

-- | The rules a run is made of, in the order they were registered.
--
-- A value rather than a constant, so a repository can add rules of its own and
-- turn others off. It is also what makes the halves of a run agree: the set
-- that produced a report is the set that renders it and judges suppressions
-- against it.
--
-- One list rather than a @Map RuleId Rule@, tempting as that looks. A rule
-- carries its own id, so a map keyed on one has a state where key and rule
-- disagree, and building it from a list is where a duplicate id would be
-- silently resolved rather than loudly refused, which is what 'ruleSet' exists
-- to do. A lookup over a hundred rules does not need an index.
newtype RuleSet = RuleSet {ruleSetEntries :: [RuleEntry]}

-- | What can stop a list of rules from being a set, as what happened rather
-- than as the sentence about it.
--
-- Every one of these is a mistake in an executable rather than in a
-- repository's code: a rule writes its own id as a literal, and the ids to turn
-- off come from a file this tool refuses before it gets here. So none of them
-- reaches a report, and 'renderRuleSetError' is the only place any of them is
-- put into words.
data RuleSetError
  = RuleIdsAreNotIds !(NonEmpty RuleId)
  | TwoRulesByOneId !(NonEmpty RuleId)
  | TurnedOffRulesDoNotExist !(NonEmpty RuleId)
  deriving (Show, Eq)

renderRuleSetError :: RuleSetError -> [Chunk]
renderRuleSetError = \case
  RuleIdsAreNotIds rids ->
    [ chunk "These rule ids are not ids: ",
      fore red (chunk (listOf rids)),
      chunk ". An id is PascalCase, alphanumeric, and holds no colon."
    ]
  TwoRulesByOneId rids ->
    [ chunk "Two rules are called ",
      fore red (chunk (listOf rids)),
      chunk ". An id is what a report prints and what explain is asked about, so it has to mean one rule."
    ]
  TurnedOffRulesDoNotExist rids ->
    [ chunk "There is no rule called ",
      fore red (chunk (listOf rids)),
      chunk ". Turning off a name nothing answers to leaves the rule you meant running."
    ]
  where
    listOf = T.intercalate ", " . map ruleIdText . NE.toList

-- | A set from the rules that are to run and the ids that are not.
--
-- Two ids the same is refused rather than resolved: an id is what a suppression
-- names, so a set with two rules by one name has two answers to what it means.
-- This is the whole of what enforces uniqueness now that ids are not an enum
-- the compiler can check.
--
-- Turning off an id nothing answers to is refused too: it is a typo in the one
-- place where a typo means the rule you meant is still running.
ruleSet :: [Rule] -> [RuleId] -> Either RuleSetError RuleSet
ruleSet rules off = do
  assertEveryIdIsWellFormed
  assertNoDuplicates
  assertEveryTurnedOffRuleExists
  pure (RuleSet (map tagged rules))
  where
    tagged r =
      RuleEntry
        { entryRule = r,
          entryUse = if ruleId r `elem` off then RuleTurnedOff else RuleRuns
        }

    -- A rule writes its own id as a literal, so this is where one that is not
    -- an id is caught: no suppression could name it, since the parser holds
    -- text to exactly this shape.
    assertEveryIdIsWellFormed =
      case [rid | r <- rules, let rid = ruleId r, not (isValid rid)] of
        [] -> Right ()
        malformed -> Left (RuleIdsAreNotIds (NE.fromList malformed))

    assertNoDuplicates =
      case [ids | ids@(_ : _ : _) <- groupSame (sort (map ruleId rules))] of
        [] -> Right ()
        (clashing : _) -> Left (TwoRulesByOneId (NE.fromList clashing))

    assertEveryTurnedOffRuleExists =
      case [rid | rid <- off, rid `notElem` map ruleId rules] of
        [] -> Right ()
        missing -> Left (TurnedOffRulesDoNotExist (NE.fromList missing))

    groupSame [] = []
    groupSame (x : xs) = let (same, rest) = span (== x) xs in (x : same) : groupSame rest

-- | No rules at all, which is what a caller with nowhere to report a refused set
-- falls back to. Building the shipped set cannot fail, and this is what says so
-- without a partial function.
emptyRuleSet :: RuleSet
emptyRuleSet = RuleSet []

-- | The rules that run, in the order they were registered.
ruleSetRules :: RuleSet -> [Rule]
ruleSetRules rs = [entryRule e | e <- ruleSetEntries rs, entryUse e == RuleRuns]

-- | The rules this repository has decided against, which it still knows: a
-- suppression naming one is wrong differently from one naming nothing.
ruleSetTurnedOff :: RuleSet -> [Rule]
ruleSetTurnedOff rs = [entryRule e | e <- ruleSetEntries rs, entryUse e == RuleTurnedOff]

-- | What this run makes of a name somebody wrote down. 'Nothing' is a name
-- nothing here answers to, which is a typo, and means the rule they meant is
-- still running.
useOf :: RuleSet -> RuleId -> Maybe RuleUse
useOf rs rid = entryUse <$> entryFor rs rid

entryFor :: RuleSet -> RuleId -> Maybe RuleEntry
entryFor rs rid = case [e | e <- ruleSetEntries rs, ruleId (entryRule e) == rid] of
  (e : _) -> Just e
  [] -> Nothing

-- | The rule an id names, when this run runs one.
--
-- 'Maybe' buys a repository the ability to add rules at all, and costs this one
-- branch, which is reachable: a report is data on disk, so one produced by
-- another rule set can be handed to this one to render.
--
-- A rule turned off is not one, which keeps its suppressions out of every
-- layer's judging. They are reported instead, by the parser or by the project
-- layer.
ruleFor :: RuleSet -> RuleId -> Maybe Rule
ruleFor rs rid = case entryFor rs rid of
  Just e | entryUse e == RuleRuns -> Just (entryRule e)
  _ -> Nothing

-- | The rule an id names whether or not this run makes it, which is what a
-- command that talks about a rule rather than running one wants: a rule turned
-- off is still a rule this build can be asked about.
ruleNamed :: RuleSet -> RuleId -> Maybe Rule
ruleNamed rs rid = entryRule <$> entryFor rs rid

rulesAtLevel :: RuleSet -> Level -> [Rule]
rulesAtLevel rs l = [r | r <- ruleSetRules rs, ruleLevel r == l]

-- | The same set with more ids turned off, which is what a repository's
-- hopinion.yaml does to whatever set the executable was built with.
withoutRules :: [RuleId] -> RuleSet -> Either RuleSetError RuleSet
withoutRules off rs =
  ruleSet
    (map entryRule (ruleSetEntries rs))
    (off ++ map ruleId (ruleSetTurnedOff rs))

-- | The scope an annotation about this comment would have to name. A comment
-- inside a declaration is about that declaration as far as the portable scope
-- key is concerned; statement precision is recovered from the span.
scopeOfComment :: ModuleRef -> CommentFact -> ScopeKey
scopeOfComment mk cf =
  case commentFactAttachment cf of
    AttachedToDecl d -> ScopeOfDecl mk d
    AttachedToStatement d _ -> ScopeOfDecl mk d
    AttachedToFile -> ScopeOfFile mk
    AttachedToExportList -> ScopeOfFile mk
    Unattached -> ScopeOfFile mk
