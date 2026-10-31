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
      let madeUp = reachesProject ownership (testLiteralsUses facts) . testLiteralConsumers
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
-- libraries instead, unless a library function is also handed one of the
-- project's own functions defined elsewhere, which it hands the value on to.
--
-- A constructor decides nothing, the project's included, because building a
-- value runs no code: what decides is what the value is then handed to, so the
-- expected side of an assertion stays exempt however its value is built. A
-- value bound to a name reaches the project when any use of the name does.
reachesProject :: (NameAt -> Ownership) -> [BindingUse] -> [Consumer] -> Bool
reachesProject owner uses = go S.empty
  where
    go :: S.Set Text -> [Consumer] -> Bool
    go visiting = \case
      [] -> False
      ConsumerCall callee others : rest
        | isConstructorName (nameAtText callee) -> go visiting rest
        | otherwise -> case owner callee of
            OwnershipThisModule -> True
            OwnershipProject -> True
            OwnershipLibrary -> any handsOnTo others || go visiting rest
      ConsumerBinding name : _
        | name `S.member` visiting -> False
        | otherwise ->
            any
              (go (S.insert name visiting) . bindingUseConsumers)
              [u | u <- uses, bindingUseName u == name]

    -- Only a name defined in another module of the project, because a name
    -- this module binds is as likely to be a value a test compares against as
    -- a function it hands on to.
    handsOnTo :: NameAt -> Bool
    handsOnTo name = case owner name of
      OwnershipThisModule -> False
      OwnershipProject -> True
      OwnershipLibrary -> False

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
