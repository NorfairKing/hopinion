{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Hopinion.Check.Project.SydtestOnly.Rule
  ( rule,
    ModuleImport (..),
    TestSuiteModule (..),
  )
where

import Data.List (nub, sortOn)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Database.Esqueleto.Experimental
import Database.Persist.TH
import Hopinion.Check.Project.SydtestOnly.Extract (importsOf)
import Hopinion.Check.Project.SydtestOnly.Fact
import Hopinion.Compiled (CompiledModules)
import Hopinion.Facts.Component
import Hopinion.Facts.Module
import Hopinion.Facts.Name
import Hopinion.Facts.Place
import Hopinion.Rule
import Hopinion.Rule.Id
import Hopinion.Rule.Prose (proseBeside)

-- | This rule's tables, defined here because this rule is the only thing that
-- writes them and the only thing that reads them.
--
-- Two facts, because the two findings ask different things: an import of
-- another framework is wrong wherever it is, while a test suite that never
-- reaches sydtest is wrong only as a whole.
-- Every import is kept, not only those of a test framework, because a suite
-- can reach sydtest through a library of the project, and only the library's
-- own imports say whether it does.
-- Every module of a test suite is kept, not only those that import something,
-- because a suite with no imports at all is the one the second finding is
-- about.
share
  [mkPersist sqlSettings, mkMigrate "sydtestOnlyMigration"]
  [persistLowerCase|
ModuleImport
    package PackageName
    moduleRef ModuleRef
    kind ComponentKind
    span Span
    imported ModuleKey
    deriving Show Eq

TestSuiteModule
    package PackageName
    moduleRef ModuleRef
    span Span
    deriving Show Eq
|]

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "TestSydtestOnly",
      ruleText = "Tests are written with sydtest.",
      ruleWhy = $(proseBeside "why.txt"),
      ruleFix = $(proseBeside "fix.txt"),
      ruleImpl =
        RuleImplProject
          ProjectCheck
            { projectCheckMigration = sydtestOnlyMigration,
              projectCheckCarry = carry,
              projectCheckFindings = findings
            }
    }

-- | Every module's imports, not only a test suite's, because the helpers that
-- tests share live in libraries too: a -gen library's test utilities are as
-- much a part of the tests as the specs that import them.
--
-- A module that is not Haskell source still counts as a module of its suite.
-- A discovery preprocessor's main module is one, and it is the module a
-- finding about the suite as a whole points at.
carry :: PackageName -> ModuleContext -> Carry
carry pkg ctx = do
  let ref = moduleContextRef ctx
  let kind = moduleContextComponent ctx
  mapM_
    insert_
    [ ModuleImport
        { moduleImportPackage = pkg,
          moduleImportModuleRef = ref,
          moduleImportKind = kind,
          moduleImportSpan = importFactSpan i,
          moduleImportImported = importFactModule i
        }
    | ModuleSyntaxParsed parsed <- [moduleContextSyntax ctx],
      i <- importsOf ctx parsed
    ]
  case kind of
    ComponentKindTest ->
      insert_
        TestSuiteModule
          { testSuiteModulePackage = pkg,
            testSuiteModuleModuleRef = ref,
            testSuiteModuleSpan = wholeFileSpan (moduleContextPath ctx)
          }
    ComponentKindLib -> pure ()
    ComponentKindApp -> pure ()
    ComponentKindBench -> pure ()

-- | Every import of another framework, and every test suite that reaches
-- sydtest neither itself nor through a library of the project.
--
-- Nothing here asks the compiler anything: which framework a module imports
-- is written in the module.
findings :: CompiledModules -> Query CheckResult
findings _ = do
  imports <- map entityVal <$> select (from (table @ModuleImport))
  suiteModules <- map entityVal <$> select (from (table @TestSuiteModule))
  let reaching = librariesReachingSydtest imports
  let sydtestUsers =
        nub
          [ suiteOf (moduleImportPackage i) (moduleImportModuleRef i)
          | i <- imports,
            moduleImportKind i == ComponentKindTest,
            reachesSydtest reaching (moduleImportImported i)
          ]
  pure
    ( findingsResult
        ( [ otherFrameworkFinding i
          | i <- sortOn moduleImportSpan imports,
            isOtherFramework (moduleImportImported i)
          ]
            ++ [ noSydtestFinding m
               | m <- suiteRepresentatives suiteModules,
                 suiteOfModule m `notElem` sydtestUsers
               ]
        )
    )

-- | Every library module of the project that imports sydtest, itself or
-- through other library modules of the project.
--
-- By module name alone, because an import does not say which package it is
-- from.
-- Two library modules of one name in different packages are therefore taken
-- for one, which can only err towards a suite reaching sydtest, never towards
-- a finding that should not be there.
librariesReachingSydtest :: [ModuleImport] -> Set ModuleKey
librariesReachingSydtest imports =
  let libraryImports :: [(ModuleKey, ModuleKey)]
      libraryImports =
        [ (moduleRefModule (moduleImportModuleRef i), moduleImportImported i)
        | i <- imports,
          moduleImportKind i == ComponentKindLib
        ]
      grow :: Set ModuleKey -> Set ModuleKey
      grow known =
        let next = S.union known (S.fromList [m | (m, imported) <- libraryImports, reachesSydtest known imported])
         in if next == known then known else grow next
   in grow S.empty

-- | Whether importing this module brings in sydtest, given the library modules
-- known to.
reachesSydtest :: Set ModuleKey -> ModuleKey -> Bool
reachesSydtest reaching imported = isWithin sydtest imported || S.member imported reaching

-- | Which test suite a module belongs to.
--
-- The package as well as the component, because cabal holds component names
-- unique per package rather than per project.
suiteOf :: PackageName -> ModuleRef -> (PackageName, ComponentName)
suiteOf pkg ref = (pkg, moduleRefComponent ref)

suiteOfModule :: TestSuiteModule -> (PackageName, ComponentName)
suiteOfModule m = suiteOf (testSuiteModulePackage m) (testSuiteModuleModuleRef m)

-- | One module per test suite, for a finding about the suite to point at: its
-- main module if it has one, since that is where a reader looks first, and
-- otherwise its first module.
suiteRepresentatives :: [TestSuiteModule] -> [TestSuiteModule]
suiteRepresentatives ms =
  [ m
  | suite <- nub (map suiteOfModule ms),
    let inSuite = [m' | m' <- sortOn testSuiteModuleSpan ms, suiteOfModule m' == suite],
    m <- take 1 ([m' | m' <- inSuite, moduleRefModule (testSuiteModuleModuleRef m') == ModuleKey "Main"] ++ inSuite)
  ]

otherFrameworkFinding :: ModuleImport -> Finding
otherFrameworkFinding i =
  Finding
    { findingRule = ruleId rule,
      findingScope = ScopeKeyOfFile (moduleImportModuleRef i),
      findingSpan = moduleImportSpan i,
      findingMessage =
        T.concat
          [ "An import of another test framework: ",
            unModuleKey (moduleImportImported i),
            "."
          ]
    }

noSydtestFinding :: TestSuiteModule -> Finding
noSydtestFinding m =
  let ref = testSuiteModuleModuleRef m
   in Finding
        { findingRule = ruleId rule,
          findingScope = ScopeKeyOfFile ref,
          findingSpan = testSuiteModuleSpan m,
          findingMessage =
            T.concat
              [ "A test suite that never reaches Test.Syd: ",
                unComponentName (moduleRefComponent ref),
                "."
              ]
        }

sydtest :: Text
sydtest = "Test.Syd"

-- | Whether a module belongs to a test framework that runs tests itself.
--
-- QuickCheck is not one: sydtest runs its properties, and genvalidity is built
-- on it.
-- tasty-bench is not one either, though it lives under tasty's name: it runs
-- benchmarks, which are not tests.
isOtherFramework :: ModuleKey -> Bool
isOtherFramework imported =
  any (`isWithin` imported) otherFrameworks
    && not (isWithin "Test.Tasty.Bench" imported)

otherFrameworks :: [Text]
otherFrameworks =
  [ -- hspec, and the libraries around it, such as hspec-expectations
    "Test.Hspec",
    -- tasty, and every tasty-* provider
    "Test.Tasty",
    "Test.HUnit",
    -- test-framework, and every test-framework-* provider
    "Test.Framework",
    -- Cabal's detailed-0.9 test interface
    "Distribution.TestSuite"
  ]

-- | Whether a module is the hierarchy's root or below it, so that a module
-- whose name merely starts with the same letters is not taken for it.
isWithin :: Text -> ModuleKey -> Bool
isWithin root (ModuleKey name) = name == root || T.isPrefixOf (T.concat [root, "."]) name
