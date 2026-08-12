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

-- | [check:ref GenValidSpecPerGenValid]
module Hopinion.Check.Project.GenValidSpecPerGenValid
  ( rule,
    ObligationMade (..),
    ObligationMet (..),
    ObligationTemplateHaskell (..),
  )
where

import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Esqueleto.Experimental
import Database.Persist.TH
import Hopinion.Compiled (CompiledModules, couldGenerateUseOf, declaredInstancesOf)
import Hopinion.Facts
import Hopinion.Rule
import Hopinion.Rule.Id
import Hopinion.Store

-- | This rule's tables, defined here because this rule is the only thing that
-- writes them and the only thing that reads them.
--
-- One table per fact rather than one per rule: an obligation made and an
-- obligation met are different things, found in different packages, and the
-- check is the left join between them. "This module holds code nothing can
-- read" is a third fact rather than a column of either, and is read against
-- both sides of the join.
--
-- No keys. A fact is a row, two instances in one module are two rows, and there
-- is nothing to overwrite.
share
  [mkPersist sqlSettings, mkMigrate "obligationMigration"]
  [persistLowerCase|
ObligationMade
    package PackageName
    moduleRef ModuleRef
    kind ComponentKind
    span Span
    decl DeclName
    typeHead TypeHead
    deriving Show Eq

ObligationMet
    package PackageName
    kind ComponentKind
    typeHead TypeHead
    deriving Show Eq

ObligationTemplateHaskell
    package PackageName
    moduleRef ModuleRef
    kind ComponentKind
    file StoredPath
    use TemplateHaskellUse
    deriving Show Eq
|]

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "TestGenValidSpecPerGenValid",
      ruleText =
        "A GenValid instance creates an obligation: genValidSpec @T must be called\
        \ in the test suite of that package's gen package.",
      ruleWhy =
        "A generator nothing runs is a generator nobody has checked. genValid can\
        \ produce values the type's own Validity instance rejects, and\
        \ shrinkValid can shrink to them, and every property built on it inherits\
        \ that quietly: the failures it reports are about values the code was\
        \ never meant to see. genValidSpec asserts exactly the two things the\
        \ instance promises, and it is one line. The spec belongs to the package\
        \ that declares the instance, because a spec written next door disappears\
        \ the day that other package stops mentioning the type.",
      ruleImpl =
        ProjectRule
          ProjectCheck
            { projectCheckMigration = obligationMigration,
              projectCheckCarry = carry,
              projectCheckFindings = findings
            }
    }

obligationClass :: Text
obligationClass = "GenValid"

obligationCombinator :: Text
obligationCombinator = "genValidSpec"

carry :: PackageName -> ModuleContext -> Carry
carry pkg ctx = do
  mapM_ made [i | i <- moduleContextInstances ctx, instanceFactClass i == obligationClass]
  mapM_
    met
    [ typeAppFactHead ta
    | ta <- moduleContextTypeApps ctx,
      typeAppFactFunction ta == obligationCombinator
    ]
  let record :: TemplateHaskellUse -> Carry
      record use =
        insert_
          ObligationTemplateHaskell
            { obligationTemplateHaskellPackage = pkg,
              obligationTemplateHaskellModuleRef = ref,
              obligationTemplateHaskellKind = kind,
              obligationTemplateHaskellFile = StoredPath (moduleContextPath ctx),
              obligationTemplateHaskellUse = use
            }
  case moduleContextTemplateHaskell ctx of
    NoTemplateHaskell -> pure ()
    UsesQuasiQuotes -> record UsesQuasiQuotes
    UsesSplices -> record UsesSplices
  where
    ref = moduleContextRef ctx
    kind = moduleContextComponent ctx

    made inst =
      insert_
        ObligationMade
          { obligationMadePackage = pkg,
            obligationMadeModuleRef = ref,
            obligationMadeKind = kind,
            obligationMadeSpan = instanceFactSpan inst,
            obligationMadeDecl = declOf (instanceFactScope inst),
            obligationMadeTypeHead = instanceFactType inst
          }

    met th =
      insert_
        ObligationMet
          { obligationMetPackage = pkg,
            obligationMetKind = kind,
            obligationMetTypeHead = th
          }

    declOf sk = case sk of
      ScopeOfDecl _ d -> d
      ScopeOfFile _ -> DeclName ""

findings :: CompiledModules -> Query CheckResult
findings compiled = do
  names <- packageNames
  mconcat <$> traverse (perPackage compiled) names

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
perPackage :: CompiledModules -> PackageName -> Query CheckResult
perPackage compiled pkg = do
  generating <- libraryModulesUsingTemplateHaskell pkg
  mapM_ (recordGeneratedObligations compiled pkg) generating
  home <- genPackageFor pkg
  case home of
    NoGenPackage gen -> do
      unmet <- obligationsUnmetIn pkg gen
      pure (findingsResult (map (findingFor (NoGenPackage gen)) unmet))
    GenPackage gen -> do
      unmet <- obligationsUnmetIn pkg gen
      splicing <- splicingTestModulesOf gen
      reportable <- filterM (notGeneratedIn compiled splicing) unmet
      pure (findingsResult (map (findingFor (GenPackage gen)) reportable))

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
recordGeneratedObligations :: CompiledModules -> PackageName -> ObligationTemplateHaskell -> Query ()
recordGeneratedObligations compiled pkg s = do
  declared <- liftIO (declaredInstancesOf (storedPathFile (obligationTemplateHaskellFile s)) compiled)
  case declared of
    -- No build spoke for this module, so what it generates stays unknown and
    -- the source is all there is. Only a run with no artifacts at all gets
    -- here, since a run with them fails on a module they do not cover.
    Nothing -> pure ()
    Just instances -> do
      written <- typeHeadsMadeIn pkg (obligationTemplateHaskellModuleRef s)
      mapM_
        (madeByGeneration s)
        [ TypeHead (declaredInstanceType i)
        | i <- instances,
          declaredInstanceClass i == obligationClass,
          TypeHead (declaredInstanceType i) `notElem` written
        ]
  where
    madeByGeneration :: ObligationTemplateHaskell -> TypeHead -> Query ()
    madeByGeneration source th =
      insert_
        ObligationMade
          { obligationMadePackage = pkg,
            obligationMadeModuleRef = obligationTemplateHaskellModuleRef source,
            obligationMadeKind = obligationTemplateHaskellKind source,
            obligationMadeSpan = wholeFileSpan (storedPathFile (obligationTemplateHaskellFile source)),
            obligationMadeDecl = DeclName (T.concat ["instance ", obligationClass, " ", typeHeadText th]),
            obligationMadeTypeHead = th
          }

-- | Which types this module already made an obligation about, which is what
-- the compiler's list is measured against.
typeHeadsMadeIn :: PackageName -> ModuleRef -> Query [TypeHead]
typeHeadsMadeIn pkg ref =
  map unValue
    <$> select
      ( do
          made <- from (table @ObligationMade)
          where_ (made ^. ObligationMadePackage ==. val pkg)
          where_ (made ^. ObligationMadeModuleRef ==. val ref)
          pure (made ^. ObligationMadeTypeHead)
      )

-- | The rule itself: an instance in this package's library with no call to the
-- combinator anywhere in the gen package's test suite.
--
-- A left join and a null check, which is what "this is required and it is
-- missing" is, and the five obligation rules still to come are this same query
-- with another class and another combinator in it.
--
-- The join is on the type head alone, which is deliberately lossy for the
-- reason 'Hopinion.Facts.Name.TypeHead' gives: @genValidSpec \@(Allowed (Set
-- Int))@ meets the obligation of every @Allowed@.
obligationsUnmetIn :: PackageName -> PackageName -> Query [ObligationMade]
obligationsUnmetIn pkg gen =
  map entityVal
    <$> select
      ( do
          (made :& met) <-
            from
              ( table @ObligationMade
                  `leftJoin` table @ObligationMet
                    `on` ( \(made :& met) ->
                             met ?. ObligationMetTypeHead ==. just (made ^. ObligationMadeTypeHead)
                               &&. met ?. ObligationMetPackage ==. just (val gen)
                               &&. met ?. ObligationMetKind ==. just (val ComponentTest)
                         )
              )
          where_ (made ^. ObligationMadePackage ==. val pkg)
          where_ (made ^. ObligationMadeKind ==. val ComponentLib)
          where_ (isNothing (met ?. ObligationMetTypeHead))
          orderBy [asc (made ^. ObligationMadeModuleRef), asc (made ^. ObligationMadeId)]
          pure made
      )

-- | Whether this obligation's type was not tested by anything a splicing test
-- module generated, which is what makes it safe to report.
--
-- Asked of every test module that splices, since the call could have been
-- generated in any of them, and only answered by all of them saying no.
notGeneratedIn :: CompiledModules -> [ObligationTemplateHaskell] -> ObligationMade -> Query Bool
notGeneratedIn compiled splicing made =
  not
    <$> anyM
      ( \s ->
          liftIO
            ( couldGenerateUseOf
                obligationCombinator
                (typeHeadText (obligationMadeTypeHead made))
                (storedPathFile (obligationTemplateHaskellFile s))
                compiled
            )
      )
      splicing

-- | Short-circuiting, because the first test module whose generated code names
-- both is the whole answer and reading the rest is reading files for nothing.
anyM :: (a -> Query Bool) -> [a] -> Query Bool
anyM _ [] = pure False
anyM f (x : xs) = do
  here <- f x
  if here then pure True else anyM f xs

-- | Every module of this package's library that uses Template Haskell at all,
-- which is what threatens the set of instances the library declares: a
-- quasiquote generates instances as readily as a splice does, and
-- @persistLowerCase@ is the corpus's commonest source of unseeable instances.
libraryModulesUsingTemplateHaskell :: PackageName -> Query [ObligationTemplateHaskell]
libraryModulesUsingTemplateHaskell pkg =
  map entityVal
    <$> select
      ( do
          generating <- from (table @ObligationTemplateHaskell)
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
splicingTestModulesOf :: PackageName -> Query [ObligationTemplateHaskell]
splicingTestModulesOf pkg =
  map entityVal
    <$> select
      ( do
          splicing <- from (table @ObligationTemplateHaskell)
          where_ (splicing ^. ObligationTemplateHaskellPackage ==. val pkg)
          where_ (splicing ^. ObligationTemplateHaskellKind ==. val ComponentTest)
          where_ (splicing ^. ObligationTemplateHaskellUse ==. val UsesSplices)
          orderBy [asc (splicing ^. ObligationTemplateHaskellModuleRef)]
          pure splicing
      )

-- | The home is in the message because the two cases want different things
-- done: one is a test to write, the other is a package to create first.
findingFor :: GenPackage -> ObligationMade -> Finding
findingFor home m =
  Finding
    { findingRule = ruleId rule,
      findingScope =
        ScopeOfDecl
          (obligationMadeModuleRef m)
          (obligationMadeDecl m),
      findingSpan = obligationMadeSpan m,
      findingMessage =
        T.pack
          ( unwords
              ( [ "No",
                  T.unpack obligationCombinator,
                  "@" ++ T.unpack (typeHeadText (obligationMadeTypeHead m))
                ]
                  ++ case home of
                    GenPackage gen -> ["anywhere in", T.unpack (packageNameText gen) ++ "'s test suite."]
                    NoGenPackage gen ->
                      [ "anywhere, and there is no",
                        T.unpack (packageNameText gen),
                        "for it to be written in."
                      ]
              )
          )
    }
