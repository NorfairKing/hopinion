{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Test.NoDummyValues.Rule (rule) where

import Data.Char (isDigit, isUpper)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Hopinion.Check.Test.NoDummyValues.Fact
import Hopinion.Compiled (nameOriginsOf)
import Hopinion.Facts.Module
import Hopinion.Facts.Name
import Hopinion.Facts.Place
import Hopinion.Hie (HiePosition (..), NameOrigin (..))
import Hopinion.Rule
import Hopinion.Rule.Id

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "TestNoDummyValues",
      ruleText = "A test runs on generated values, not on literals made up for it.",
      ruleWhy =
        "A made-up value tests one input and reads as if it tested all of them.\n\
        \The ideal replacement is a property test: quantify over the value with\n\
        \forAllValid, so the test states the claim it is making and the\n\
        \generator finds the inputs nobody thought to write down.\n\
        \A literal counts when it is handed to this project's own code, the code\n\
        \under test. One handed to a library configures that library instead,\n\
        \like a timeout, a header name or the expected side of an assertion.\n\
        \Binding the literal somewhere else does not make it less made up, so a\n\
        \value is followed into every binding of this module the test uses.\n\
        \When the literals are the point of the test, such as a regression input\n\
        \or the edge case a bug was found at, suppress it and say why: one\n\
        \suppression above a test answers for every literal in it.",
      ruleImpl = RuleImplModule (ModuleCheckFromBuild check)
    }

-- | One finding per test, or per binding outside every test, naming every
-- literal in it that reaches the project's own code and pointing at the first.
--
-- Per test rather than per literal, because what a suppression says is why a
-- test is written with the values it has, and a test whose values are the
-- point usually has several of them.
check :: ModuleBuild -> ModuleContext -> IO CheckResult
check build ctx
  | not (moduleContextIsSpecFile ctx) = pure noResult
  | otherwise = do
      origins <- nameOriginsOf (moduleContextPath ctx) (moduleBuildCompiled build)
      let facts = moduleContextTestLiterals ctx
      let ownership = ownershipOf (moduleBuildProjectPackages build) origins
      let madeUp = reachesProject ownership facts . testLiteralConsumers
      pure $
        findingsResult
          [ Finding
              { findingRule = ruleId rule,
                findingScope = testLiteralScope first,
                findingSpan = testLiteralSpan first,
                findingMessage =
                  T.pack
                    ( concat
                        [ "Literals a test runs on: ",
                          T.unpack (T.intercalate ", " (map testLiteralText (NE.toList literals))),
                          "."
                        ]
                    )
              }
          | literals@(first :| _) <- ownedTogether (filter madeUp (testLiteralsLiterals facts))
          ]

-- | Whose code a name refers to: this module's, another module of this
-- project's, or a library's.
data Ownership
  = OwnershipThisModule
  | OwnershipProject
  | OwnershipLibrary

-- | How a value got where it is: written there, or returned by a library
-- function it was handed to.
data Passage
  = PassageDirect
  | PassageThroughLibrary

-- | Whether a function this module defines can be followed into at a
-- parameter, which it can when it names that parameter.
data Followability
  = FollowabilityNamed
  | FollowabilityUnknown

-- | Whose code each name is, by where the build says it is defined.
--
-- This module and every package of this repository are the project; anything
-- else is a library. Without a build there is only the source to go on, and
-- the source says only what this module defines itself, so everything else is
-- taken for a library.
ownershipOf :: [PackageName] -> Maybe (M.Map HiePosition NameOrigin) -> NameAt -> Ownership
ownershipOf project origins name = case origins >>= M.lookup (hiePositionOf (nameAtPosition name)) of
  Just NameOriginThisModule -> OwnershipThisModule
  Just (NameOriginUnit unit)
    | any (`isUnitOf` unit) project -> OwnershipProject
    | otherwise -> OwnershipLibrary
  Nothing -> case nameAtDefinition name of
    DefinitionHere -> OwnershipThisModule
    DefinitionElsewhere -> OwnershipLibrary

hiePositionOf :: Position -> HiePosition
hiePositionOf p =
  HiePosition
    { hiePositionLine = positionLine p,
      hiePositionCol = positionCol p
    }

-- | Whether a unit is a build of this package: its name, then a version.
isUnitOf :: PackageName -> Text -> Bool
isUnitOf pkg unit =
  unit == unPackageName pkg || case T.stripPrefix (T.snoc (unPackageName pkg) '-') unit of
    Just rest -> maybe False (isDigit . fst) (T.uncons rest)
    Nothing -> False

-- | Whether a value with these consumers reaches the project's own code.
--
-- It does when any function among them is the project's: whatever library code
-- it passes through on the way, a value handed on to the project's code is an
-- input to it. One that only library functions receive configures those
-- libraries instead. A library function handed one of the project's functions
-- alongside the value, as in @map f xs@, may hand the value on to it, but
-- telling such a function from a value of the project's needs types, so it is
-- not followed.
--
-- A constructor decides nothing, the project's included, because building a
-- value runs no code: what decides is what the value is then handed to, so the
-- expected side of an assertion stays exempt however its value is built. A
-- value bound to a name reaches the project when any use of the name does.
--
-- A function this module defines is followed into: the value reaches the
-- project when the parameter it is handed as does inside that function, or
-- when what the function returns does further out. One whose parameters this
-- module does not name, such as one written point-free, cannot be followed
-- and is taken for the code under test.
reachesProject :: (NameAt -> Ownership) -> TestLiterals -> [Consumer] -> Bool
reachesProject owner facts = go PassageDirect S.empty S.empty
  where
    go :: Passage -> S.Set Text -> S.Set (Text, Word) -> [Consumer] -> Bool
    go passage bindings parameters = \case
      [] -> False
      ConsumerCall callee index : rest
        | isConstructorName (nameAtText callee) -> go passage bindings parameters rest
        | otherwise -> case owner callee of
            OwnershipThisModule -> case followable (nameAtText callee) index of
              FollowabilityNamed -> intoFunction bindings parameters (nameAtText callee) index || go passage bindings parameters rest
              FollowabilityUnknown -> True
            OwnershipProject -> True
            OwnershipLibrary -> go PassageThroughLibrary bindings parameters rest
      ConsumerBinding name : _ -> case passage of
        -- What a library returns is its own value, which the argument may or
        -- may not be part of: the path to a file is not what was read from it.
        -- Handed straight on as an argument it is followed; bound to a name
        -- for later, it is not.
        PassageThroughLibrary -> False
        PassageDirect
          | name `S.member` bindings -> False
          | otherwise ->
              any
                (go PassageDirect (S.insert name bindings) parameters . bindingUseConsumers)
                [u | u <- testLiteralsUses facts, bindingUseName u == name]

    -- Inside the function, where the value is a parameter. What the function
    -- returns is followed by the caller, so the function's own name ends the
    -- path here rather than leading on to its uses.
    intoFunction :: S.Set Text -> S.Set (Text, Word) -> Text -> Word -> Bool
    intoFunction bindings parameters function index
      | (function, index) `S.member` parameters = False
      | otherwise =
          any
            (go PassageDirect (S.insert function bindings) (S.insert (function, index) parameters) . parameterUseConsumers)
            [ u
            | u <- testLiteralsParameters facts,
              parameterUseFunction u == function,
              parameterUseIndex u == index
            ]

    followable :: Text -> Word -> Followability
    followable function index
      | any (\a -> functionArityName a == function && index < functionArityParameters a) (testLiteralsArities facts) = FollowabilityNamed
      | otherwise = FollowabilityUnknown

isConstructorName :: Text -> Bool
isConstructorName name = case T.uncons name of
  Just (c, _) -> isUpper c || c == ':'
  Nothing -> False

-- | The literals of each owner, in source order, owners in the order of their
-- first literal.
ownedTogether :: [TestLiteral] -> [NonEmpty TestLiteral]
ownedTogether ls =
  sortOn
    (testLiteralSpan . NE.head)
    [ NE.sortWith testLiteralSpan group
    | group <- M.elems (M.fromListWith NE.append [(testLiteralOwner l, l :| []) | l <- ls])
    ]
