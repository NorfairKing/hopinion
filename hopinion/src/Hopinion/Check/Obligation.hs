{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The rules that say required code must exist: a set of instances in a
-- package's library obliges a call in the test suite of that package's gen
-- package.
--
-- One engine and one set of tables for the whole family rather than one of each
-- per rule, which is the one place a rule does not own its own table. What
-- would be private to each is the same three columns, the same join and the
-- same two questions put to the compiler, and a rule that had to restate all of
-- that to say "ToJSON and FromJSON, jsonSpec" would be four hundred lines of
-- agreement waiting to drift. The rule id is a column, so a row still belongs
-- to exactly one rule and no rule can read another's.
module Hopinion.Check.Obligation
  ( Obligation (..),
    obligationRule,
    ObligationMade (..),
    ObligationMet (..),
    ObligationTemplateHaskell (..),
  )
where

import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Data.List (nubBy)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Database.Esqueleto.Experimental
import Database.Persist.TH
import Hopinion.Compiled (CompiledModules, couldGenerateUseOf, declaredInstancesOf)
import Hopinion.Facts.Component
import Hopinion.Facts.Instance
import Hopinion.Facts.Module
import Hopinion.Facts.Name
import Hopinion.Facts.Package
import Hopinion.Facts.Place
import Hopinion.Facts.TemplateHaskell
import Hopinion.Facts.TypeApp
import Hopinion.Hie (DeclaredInstance (..))
import Hopinion.Rule
import Hopinion.Rule.Id
import Hopinion.Store

-- | The family's tables.
--
-- [allow:file:TestJsonSpecPerJsonType] The quasi-quoter below writes ToJSON
-- and FromJSON for the Key of every table it makes, and every table in the
-- repository shares one Key type head, so this answers for all of them.
-- Nothing here is a wire format: a Key is a row number that never leaves the
-- store it came from, and persistent tests its own instances.
--
-- One table per fact rather than one per rule: an obligation made and an
-- obligation met are different things, found in different packages, and the
-- check is the join between them. "This module holds code nothing can read" is
-- a third fact rather than a column of either, and is read against both sides.
--
-- No keys beyond the row's own. A fact is a row, two instances in one module
-- are two rows, and there is nothing to overwrite.
share
  [mkPersist sqlSettings, mkMigrate "obligationMigration"]
  [persistLowerCase|
ObligationMade
    rule RuleId
    package PackageName
    moduleRef ModuleRef
    kind ComponentKind
    span Span
    decl DeclName
    className Text
    typeHead TypeHead
    deriving Show Eq

ObligationMet
    rule RuleId
    package PackageName
    kind ComponentKind
    typeHead TypeHead
    deriving Show Eq

ObligationTemplateHaskell
    rule RuleId
    package PackageName
    moduleRef ModuleRef
    kind ComponentKind
    file StoredPath
    use TemplateHaskellUse
    deriving Show Eq
|]

-- | What one obligation rule is: a set of classes, and the call their presence
-- requires.
--
-- Every class rather than any of them, because what the combinator checks is
-- the set together: @jsonSpec@ asserts that encoding then decoding gets the
-- value back, which is a claim about @ToJSON@ and @FromJSON@ jointly and is
-- not a claim a type with only one of them can make.
data Obligation = Obligation
  { obligationId :: !RuleId,
    obligationText :: !Text,
    obligationWhy :: !Text,
    obligationClasses :: !(NonEmpty Text),
    obligationCombinator :: !Text
  }

obligationRule :: Obligation -> Rule
obligationRule o =
  Rule
    { ruleId = obligationId o,
      ruleText = obligationText o,
      ruleWhy = obligationWhy o,
      ruleImpl =
        ProjectRule
          ProjectCheck
            { projectCheckMigration = obligationMigration,
              projectCheckCarry = carry o,
              projectCheckFindings = findings o
            }
    }

carry :: Obligation -> PackageName -> ModuleContext -> Carry
carry o pkg ctx = do
  mapM_
    made
    [ i
    | i <- moduleContextInstances ctx,
      instanceFactClass i `elem` NE.toList (obligationClasses o)
    ]
  mapM_
    met
    [ typeAppFactHead ta
    | ta <- moduleContextTypeApps ctx,
      typeAppFactFunction ta == obligationCombinator o
    ]
  case moduleContextTemplateHaskell ctx of
    NoTemplateHaskell -> pure ()
    UsesQuasiQuotes -> generates UsesQuasiQuotes
    UsesSplices -> generates UsesSplices
  where
    ref = moduleContextRef ctx
    kind = moduleContextComponent ctx

    made :: InstanceFact -> Carry
    made inst =
      insert_
        ObligationMade
          { obligationMadeRule = obligationId o,
            obligationMadePackage = pkg,
            obligationMadeModuleRef = ref,
            obligationMadeKind = kind,
            obligationMadeSpan = instanceFactSpan inst,
            obligationMadeDecl = declOf (instanceFactScope inst),
            obligationMadeClassName = instanceFactClass inst,
            obligationMadeTypeHead = instanceFactType inst
          }

    met :: TypeHead -> Carry
    met th =
      insert_
        ObligationMet
          { obligationMetRule = obligationId o,
            obligationMetPackage = pkg,
            obligationMetKind = kind,
            obligationMetTypeHead = th
          }

    generates :: TemplateHaskellUse -> Carry
    generates use =
      insert_
        ObligationTemplateHaskell
          { obligationTemplateHaskellRule = obligationId o,
            obligationTemplateHaskellPackage = pkg,
            obligationTemplateHaskellModuleRef = ref,
            obligationTemplateHaskellKind = kind,
            obligationTemplateHaskellFile = StoredPath (moduleContextPath ctx),
            obligationTemplateHaskellUse = use
          }

    declOf :: ScopeKey -> DeclName
    declOf = \case
      ScopeOfDecl _ d -> d
      ScopeOfFile _ -> DeclName ""

findings :: Obligation -> CompiledModules -> Query CheckResult
findings o compiled = do
  names <- packageNames
  mconcat <$> traverse (perPackage o compiled) names

-- | Every obligation this package makes and does not meet. The two halves are
-- threatened by generated code in different ways.
--
-- An instance written out in a module that also splices is still an instance,
-- so the obligation it makes is reported whatever else that module generates,
-- and what the module adds on top comes from the compiler in
-- 'recordGeneratedObligations'.
--
-- Absence is the fragile half: the rule concludes once that no call is anywhere
-- in the gen package's test suite, and generated code there can make the call.
-- So each unmet obligation is put to the compiler first.
--
-- A package with no gen package has nowhere for its tests to live, so every
-- obligation it makes is unmet and each is reported where it was made.
-- Reporting the missing package once would have nowhere to write a
-- suppression, since a cabal file carries no annotations.
perPackage :: Obligation -> CompiledModules -> PackageName -> Query CheckResult
perPackage o compiled pkg = do
  generating <- libraryModulesUsingTemplateHaskell o pkg
  mapM_ (recordGeneratedObligations o compiled pkg) generating
  home <- genPackageFor pkg
  unmet <- obligationsUnmetIn o pkg (genPackageName home)
  reportable <- case home of
    NoGenPackage _ -> pure unmet
    GenPackage gen -> do
      splicing <- splicingTestModulesOf o gen
      filterM (notGeneratedIn o compiled splicing) unmet
  pure (findingsResult (map (findingFor o home) reportable))

genPackageName :: GenPackage -> PackageName
genPackageName = \case
  NoGenPackage n -> n
  GenPackage n -> n

-- | The obligations a module makes by generating an instance, which the source
-- cannot see and the compiler wrote down.
--
-- A module's interface lists what it declares however it came to declare it, so
-- subtracting what the source already recorded leaves exactly the generated
-- ones. They go into the same table the source side writes, so the join, the
-- message and the suppression are the same code either way.
--
-- The span is the whole file. A generated instance is at no line: which of the
-- instances a splice produced belongs to which part of it is what a @.hie@ file
-- forgets, and a suppression against the file is what a person would write.
recordGeneratedObligations :: Obligation -> CompiledModules -> PackageName -> ObligationTemplateHaskell -> Query ()
recordGeneratedObligations o compiled pkg s = do
  declared <- liftIO (declaredInstancesOf (storedPathFile (obligationTemplateHaskellFile s)) compiled)
  case declared of
    -- No build spoke for this module, so what it generates stays unknown and
    -- the source is all there is. Only a run with no artifacts at all gets
    -- here, since a run with them fails on a module they do not cover.
    Nothing -> pure ()
    Just instances -> do
      written <- classesAndTypesMadeIn o pkg (obligationTemplateHaskellModuleRef s)
      mapM_
        (madeByGeneration s)
        [ (declaredInstanceClass i, TypeHead (declaredInstanceType i))
        | i <- instances,
          declaredInstanceClass i `elem` NE.toList (obligationClasses o),
          (declaredInstanceClass i, TypeHead (declaredInstanceType i)) `notElem` written
        ]
  where
    madeByGeneration :: ObligationTemplateHaskell -> (Text, TypeHead) -> Query ()
    madeByGeneration source (cls, th) =
      insert_
        ObligationMade
          { obligationMadeRule = obligationId o,
            obligationMadePackage = pkg,
            obligationMadeModuleRef = obligationTemplateHaskellModuleRef source,
            obligationMadeKind = obligationTemplateHaskellKind source,
            obligationMadeSpan = wholeFileSpan (storedPathFile (obligationTemplateHaskellFile source)),
            obligationMadeDecl = DeclName (T.pack (unwords ["instance", T.unpack cls, T.unpack (typeHeadText th)])),
            obligationMadeClassName = cls,
            obligationMadeTypeHead = th
          }

-- | Which instances this module already made an obligation about, which is
-- what the compiler's list is measured against.
classesAndTypesMadeIn :: Obligation -> PackageName -> ModuleRef -> Query [(Text, TypeHead)]
classesAndTypesMadeIn o pkg ref =
  map (\(Value cls, Value th) -> (cls, th))
    <$> select
      ( do
          made <- from (table @ObligationMade)
          where_ (made ^. ObligationMadeRule ==. val (obligationId o))
          where_ (made ^. ObligationMadePackage ==. val pkg)
          where_ (made ^. ObligationMadeModuleRef ==. val ref)
          pure (made ^. ObligationMadeClassName, made ^. ObligationMadeTypeHead)
      )

-- | The rule itself: a type in this package's library with every class the
-- obligation names and no call to the combinator anywhere in the gen package's
-- test suite.
--
-- One finding per type rather than per instance, because one call meets the
-- obligation of all of them: a second finding about the same type would be one
-- whose suppression a reader could only write by making the first one answer
-- for nothing.
--
-- The match is on the type head alone, which is deliberately lossy for the
-- reason 'Hopinion.Facts.Name.TypeHead' gives: @genValidSpec \@(Allowed (Set
-- Int))@ meets the obligation of every @Allowed@.
obligationsUnmetIn :: Obligation -> PackageName -> PackageName -> Query [ObligationMade]
obligationsUnmetIn o pkg gen = do
  made <- madeInLibraryOf o pkg
  met <- metInTestSuiteOf o gen
  let hasEveryClass m =
        let classes = [obligationMadeClassName x | x <- made, obligationMadeTypeHead x == obligationMadeTypeHead m]
         in all (`elem` classes) (NE.toList (obligationClasses o))
  pure
    [ m
    | m <- nubBy (\a b -> obligationMadeTypeHead a == obligationMadeTypeHead b) made,
      hasEveryClass m,
      obligationMadeTypeHead m `notElem` met
    ]

-- | Ordered, because the order rows come back in is the order findings are
-- reported in, and the first row for a type is the one reported.
madeInLibraryOf :: Obligation -> PackageName -> Query [ObligationMade]
madeInLibraryOf o pkg =
  map entityVal
    <$> select
      ( do
          made <- from (table @ObligationMade)
          where_ (made ^. ObligationMadeRule ==. val (obligationId o))
          where_ (made ^. ObligationMadePackage ==. val pkg)
          where_ (made ^. ObligationMadeKind ==. val ComponentLib)
          orderBy [asc (made ^. ObligationMadeModuleRef), asc (made ^. ObligationMadeId)]
          pure made
      )

metInTestSuiteOf :: Obligation -> PackageName -> Query [TypeHead]
metInTestSuiteOf o gen =
  map unValue
    <$> select
      ( do
          met <- from (table @ObligationMet)
          where_ (met ^. ObligationMetRule ==. val (obligationId o))
          where_ (met ^. ObligationMetPackage ==. val gen)
          where_ (met ^. ObligationMetKind ==. val ComponentTest)
          pure (met ^. ObligationMetTypeHead)
      )

-- | Whether this obligation's type was not tested by anything a splicing test
-- module generated, which is what makes it safe to report.
--
-- Asked of every test module that splices, since the call could have been
-- generated in any of them, and only answered by all of them saying no.
notGeneratedIn :: Obligation -> CompiledModules -> [ObligationTemplateHaskell] -> ObligationMade -> Query Bool
notGeneratedIn o compiled splicing made =
  not
    <$> anyM
      ( \s ->
          liftIO
            ( couldGenerateUseOf
                (obligationCombinator o)
                (typeHeadText (obligationMadeTypeHead made))
                (storedPathFile (obligationTemplateHaskellFile s))
                compiled
            )
      )
      splicing

-- | Short-circuiting, because the first test module whose generated code names
-- both is the whole answer and reading the rest is reading files for nothing.
anyM :: (a -> Query Bool) -> [a] -> Query Bool
anyM f = \case
  [] -> pure False
  (x : xs) -> do
    here <- f x
    if here then pure True else anyM f xs

-- | Every module of this package's library that uses Template Haskell at all,
-- which is what threatens the set of instances the library declares: a
-- quasiquote generates instances as readily as a splice does, and
-- @persistLowerCase@ is the corpus's commonest source of unseeable instances.
libraryModulesUsingTemplateHaskell :: Obligation -> PackageName -> Query [ObligationTemplateHaskell]
libraryModulesUsingTemplateHaskell o pkg =
  map entityVal
    <$> select
      ( do
          generating <- from (table @ObligationTemplateHaskell)
          where_ (generating ^. ObligationTemplateHaskellRule ==. val (obligationId o))
          where_ (generating ^. ObligationTemplateHaskellPackage ==. val pkg)
          where_ (generating ^. ObligationTemplateHaskellKind ==. val ComponentLib)
          orderBy [asc (generating ^. ObligationTemplateHaskellModuleRef)]
          pure generating
      )

-- | Whether anything in this package's test suites splices, which is what
-- threatens the conclusion that a call appears nowhere. Whether, rather than
-- which: the conclusion is drawn once for the whole suite.
--
-- A quasiquote is not enough to doubt it. Its expansion is a function of a body
-- written in the file, so a test suite whose only Template Haskell is
-- @[relfile|foo.txt|]@ is one whose calls are all written down. Measured rather
-- than assumed: treating quasiquotes as blinding silenced every obligation the
-- rule finds on two corpus repositories, including ones confirmed by hand as
-- genuinely untested.
splicingTestModulesOf :: Obligation -> PackageName -> Query [ObligationTemplateHaskell]
splicingTestModulesOf o pkg =
  map entityVal
    <$> select
      ( do
          splicing <- from (table @ObligationTemplateHaskell)
          where_ (splicing ^. ObligationTemplateHaskellRule ==. val (obligationId o))
          where_ (splicing ^. ObligationTemplateHaskellPackage ==. val pkg)
          where_ (splicing ^. ObligationTemplateHaskellKind ==. val ComponentTest)
          where_ (splicing ^. ObligationTemplateHaskellUse ==. val UsesSplices)
          orderBy [asc (splicing ^. ObligationTemplateHaskellModuleRef)]
          pure splicing
      )

-- | The home is in the message because the two cases want different things
-- done: one is a test to write, the other is a package to create first.
findingFor :: Obligation -> GenPackage -> ObligationMade -> Finding
findingFor o home m =
  Finding
    { findingRule = obligationId o,
      findingScope =
        ScopeOfDecl
          (obligationMadeModuleRef m)
          (obligationMadeDecl m),
      findingSpan = obligationMadeSpan m,
      findingMessage =
        T.pack
          ( unwords
              ( [ "No",
                  T.unpack (obligationCombinator o),
                  concat ["@", T.unpack (typeHeadText (obligationMadeTypeHead m))]
                ]
                  ++ case home of
                    GenPackage gen -> ["anywhere in", concat [T.unpack (packageNameText gen), "'s test suite."]]
                    NoGenPackage gen ->
                      [ "anywhere, and there is no",
                        T.unpack (packageNameText gen),
                        "for it to be written in."
                      ]
              )
          )
    }
