{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Test.NoDummyValues.Rule (rule) where

import Control.Monad.State.Strict (State, evalState, gets, modify')
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
      let ownership = ownershipOf (moduleBuildProjectPackages build) (S.fromList (testLiteralsBinders facts)) origins
      let madeUp = reachesProject ownership (binderOfIn origins) facts . testLiteralConsumers
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

-- | Whether a value has been put in a list on its way, so that what a library
-- hands a function beside it may be one of its elements.
data Containment
  = ContainmentNone
  | ContainmentElement
  deriving (Eq, Ord)

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
--
-- A name the build says is local is this module's code only when this module
-- binds it. Otherwise it is a parameter, and a function handed to the test,
-- such as a library's callback, is that library's.
ownershipOf :: [PackageName] -> S.Set Position -> Maybe (M.Map HiePosition NameOrigin) -> NameAt -> Ownership
ownershipOf project binders origins name = case origins >>= M.lookup (hiePositionOf (nameAtPosition name)) of
  Just (NameOriginThisModule (Just binder))
    | positionOf binder `S.member` binders -> OwnershipThisModule
    | otherwise -> OwnershipLibrary
  Just (NameOriginThisModule Nothing) -> case nameAtDefinition name of
    DefinitionHere -> OwnershipThisModule
    DefinitionElsewhere -> OwnershipLibrary
  Just (NameOriginUnit unit)
    | any (`isUnitOf` unit) project -> OwnershipProject
    | otherwise -> OwnershipLibrary
  Nothing -> case nameAtDefinition name of
    DefinitionHere -> OwnershipThisModule
    DefinitionElsewhere -> OwnershipLibrary

-- | The binder a name written here refers to, when the build says, as a
-- position the source would give it.
binderOfIn :: Maybe (M.Map HiePosition NameOrigin) -> Position -> Maybe Position
binderOfIn origins use = case origins >>= M.lookup (hiePositionOf use) of
  Just (NameOriginThisModule (Just binder)) -> Just (positionOf binder)
  Just (NameOriginThisModule Nothing) -> Nothing
  Just (NameOriginUnit _) -> Nothing
  Nothing -> Nothing

positionOf :: HiePosition -> Position
positionOf p =
  Position
    { positionLine = hiePositionLine p,
      positionCol = hiePositionCol p
    }

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
-- libraries instead.
--
-- A library function handed a list may hand each element on to a function it
-- is handed beside the list, as in @for_ names (\\name -> ...)@, so an element
-- is followed into a lambda handed to the same call, and into a function this
-- module defines that is. A function of the project's handed alongside, as in
-- @map f xs@, is not followed, because telling it from a value of the
-- project's needs types. Only an element is followed, because what a library
-- hands a lambda beside a value that is not in a list is usually something it
-- made of it, like a directory made from a template.
-- What a function returns from a list is no element of it, so the list ends at
-- the first function it is handed to, unless that function hands it on.
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
reachesProject :: (NameAt -> Ownership) -> (Position -> Maybe Position) -> TestLiterals -> [Consumer] -> Bool
reachesProject owner binderOf facts consumers =
  evalState
    (go PassageDirect ContainmentNone S.empty consumers)
    Searched
      { searchedBindings = S.empty,
        searchedParameters = S.empty,
        searchedHandoffs = S.empty
      }
  where
    -- Inside the functions whose binders are here, so that a function's own
    -- name ends a path that started at one of its parameters.
    go :: Passage -> Containment -> S.Set Position -> [Consumer] -> State Searched Bool
    go passage containment inside = \case
      [] -> pure False
      ConsumerElement : rest -> go passage ContainmentElement inside rest
      ConsumerCall callee index : rest
        | isConstructorName (nameAtText callee) -> go passage containment inside rest
        | otherwise -> case owner callee of
            OwnershipThisModule -> case followable callee index of
              -- Inside the function the parameter is the list itself; what the
              -- function returns is something else.
              FollowabilityNamed -> intoFunction containment inside callee index `orM` go passage ContainmentNone inside rest
              FollowabilityUnknown -> pure True
            OwnershipProject -> pure True
            -- What a library returns is not the list it was handed, so its
            -- result is no element of it.
            OwnershipLibrary ->
              go PassageThroughLibrary ContainmentNone inside rest `orM` case containment of
                ContainmentElement -> elementsHandedOn inside callee
                ContainmentNone -> pure False
      ConsumerBinding name : _ -> case passage of
        -- What a library returns is its own value, which the argument may or
        -- may not be part of: the path to a file is not what was read from it.
        -- Handed straight on as an argument it is followed; bound to a name
        -- for later, it is not.
        PassageThroughLibrary -> pure False
        PassageDirect
          | nameAtPosition name `S.member` inside -> pure False
          | otherwise ->
              firstVisit
                (nameAtPosition name, containment, inside)
                searchedBindings
                (\k s -> s {searchedBindings = S.insert k (searchedBindings s)})
                ( anyM
                    (go PassageDirect containment inside . bindingUseConsumers)
                    [ u
                    | u <- testLiteralsUses facts,
                      bindingUseName u == nameAtText name,
                      refersTo (bindingUseAt u) (nameAtPosition name)
                    ]
                )

    -- Into the functions a library call is handed beside a list, as one of its
    -- elements.
    elementsHandedOn :: S.Set Position -> NameAt -> State Searched Bool
    elementsHandedOn inside callee =
      firstVisit
        (nameAtPosition callee, inside)
        searchedHandoffs
        (\k s -> s {searchedHandoffs = S.insert k (searchedHandoffs s)})
        ( anyM
            (go PassageDirect ContainmentNone inside . lambdaUseConsumers)
            [ u
            | u <- testLiteralsLambdaUses facts,
              lambdaUseCallAt u == nameAtPosition callee,
              refersTo (lambdaUseAt u) (lambdaUseBinderAt u)
            ]
            `orM` anyM
              (uncurry (intoFunction ContainmentNone inside))
              [ (function, index)
              | h <- testLiteralsHandedNames facts,
                handedNameCallAt h == nameAtPosition callee,
                let function = handedNameName h,
                OwnershipThisModule <- [owner function],
                arity <- map functionArityParameters (aritiesOf function),
                index <- takeWhile (< arity) [0 ..]
              ]
        )

    -- Inside the function, where the value is a parameter. What the function
    -- returns is followed by the caller, so the function's own name ends the
    -- path here rather than leading on to its uses.
    intoFunction :: Containment -> S.Set Position -> NameAt -> Word -> State Searched Bool
    intoFunction containment inside callee index =
      firstVisit
        (nameAtText callee, index, containment, inside)
        searchedParameters
        (\k s -> s {searchedParameters = S.insert k (searchedParameters s)})
        ( anyM
            (go PassageDirect containment (S.union (S.fromList (map functionArityAt (aritiesOf callee))) inside) . parameterUseConsumers)
            [ u
            | u <- testLiteralsParameters facts,
              parameterUseFunction u == nameAtText callee,
              definedAt callee (parameterUseFunctionAt u),
              parameterUseIndex u == index,
              refersTo (parameterUseAt u) (parameterUseBinderAt u)
            ]
        )

    followable :: NameAt -> Word -> Followability
    followable callee index
      | any (\a -> index < functionArityParameters a) (aritiesOf callee) = FollowabilityNamed
      | otherwise = FollowabilityUnknown

    aritiesOf :: NameAt -> [FunctionArity]
    aritiesOf callee =
      [ a
      | a <- testLiteralsArities facts,
        functionArityName a == nameAtText callee,
        definedAt callee (functionArityAt a)
      ]

    -- Whether a name written at one place refers to the binder written at
    -- another. The build says which binder each name refers to; without one
    -- the spelling is all there is, and it has already matched.
    refersTo :: Position -> Position -> Bool
    refersTo use binder = maybe True (== binder) (binderOf use)

    definedAt :: NameAt -> Position -> Bool
    definedAt name = refersTo (nameAtPosition name)

-- | What a search for the project has already been through, so that it goes
-- through each once: whether a value reaches the project is whether any path
-- does, and a place already searched from has already given its answer.
--
-- Keyed by the functions the search is inside as well, because those end a
-- path, so a place reached from inside different functions can answer
-- differently.
data Searched = Searched
  { searchedBindings :: !(S.Set (Position, Containment, S.Set Position)),
    searchedParameters :: !(S.Set (Text, Word, Containment, S.Set Position)),
    searchedHandoffs :: !(S.Set (Position, S.Set Position))
  }

-- | Runs a search from a place unless it has been searched from already, in
-- which case its answer is already known and was not the project.
firstVisit :: (Ord k) => k -> (Searched -> S.Set k) -> (k -> Searched -> Searched) -> State Searched Bool -> State Searched Bool
firstVisit key seen record search = do
  already <- gets (S.member key . seen)
  if already
    then pure False
    else do
      modify' (record key)
      search

anyM :: (a -> State s Bool) -> [a] -> State s Bool
anyM f = \case
  [] -> pure False
  x : xs -> f x `orM` anyM f xs

orM :: State s Bool -> State s Bool -> State s Bool
orM a b = do
  r <- a
  if r then pure True else b

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
