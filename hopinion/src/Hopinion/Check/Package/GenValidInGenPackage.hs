{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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

-- | [check:ref GenValidInGenPackage]
module Hopinion.Check.Package.GenValidInGenPackage (rule, GeneratorFact (..)) where

import Database.Esqueleto.Experimental
import Database.Persist.TH
import Hopinion.Compiled (CompiledModules)
import Hopinion.Facts
import Hopinion.Rule
import Hopinion.Rule.Id
import Hopinion.Store

-- | This rule's table, defined here because this rule is the only thing that
-- writes it and the only thing that reads it.
--
-- One row per module that has a generator, which is all this reports on: the
-- fix is to move the module's generators rather than each instance separately,
-- so only the first instance of a module is written.
share
  [mkPersist sqlSettings, mkMigrate "generatorMigration"]
  [persistLowerCase|
GeneratorFact
    package PackageName
    moduleRef ModuleRef
    kind ComponentKind
    span Span
    decl DeclName
    deriving Show Eq
|]

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsGenValidInGenPackage",
      ruleText =
        "A GenValid instance in a package's own library gets compiled into the\
        \ executable. Move it to the matching -gen package.",
      ruleWhy =
        "A generator is test code, and an instance in the library is linked into\
        \ everything that depends on the library, so the shipped executable\
        \ carries QuickCheck and the generators with it. The -gen package exists\
        \ so that dependency stops at the test suite, which is also what keeps a\
        \ generator free to be as slow or as elaborate as the property needs.",
      ruleImpl =
        PackageRule
          PackageCheck
            { packageCheckMigration = generatorMigration,
              packageCheckCarry = carry,
              packageCheckFindings = findings
            }
    }

carry :: PackageName -> ModuleContext -> Carry
carry pkg ctx =
  case [i | i <- moduleContextInstances ctx, instanceFactClass i == "GenValid"] of
    [] -> pure ()
    (i : _) ->
      insert_
        GeneratorFact
          { generatorFactPackage = pkg,
            generatorFactModuleRef = moduleContextRef ctx,
            generatorFactKind = moduleContextComponent ctx,
            generatorFactSpan = instanceFactSpan i,
            generatorFactDecl = declOf (instanceFactScope i)
          }
  where
    declOf sk = case sk of
      ScopeOfDecl _ d -> d
      ScopeOfFile _ -> DeclName ""

-- | A generator in the library of a package whose role is main, which is the
-- whole rule as one query. A gen package is where generators belong, so joining
-- the envelope's own table for the role is what makes it the one package this
-- says nothing about.
--
-- Nothing here asks the compiler anything: an instance in the wrong package is
-- one that is written down, and where it is written is all of this.
findings :: PackageName -> CompiledModules -> Query CheckResult
findings pkg _ = do
  generators <-
    select
      ( do
          (g :& p) <-
            from
              ( table @GeneratorFact
                  `innerJoin` table @StoredPackage
                    `on` (\(g :& p) -> g ^. GeneratorFactPackage ==. p ^. StoredPackageName)
              )
          where_ (g ^. GeneratorFactPackage ==. val pkg)
          where_ (g ^. GeneratorFactKind ==. val ComponentLib)
          where_ (p ^. StoredPackageRole ==. val RoleMain)
          orderBy [asc (g ^. GeneratorFactModuleRef)]
          pure g
      )
  pure (findingsResult (map (findingFor . entityVal) generators))

findingFor :: GeneratorFact -> Finding
findingFor g =
  Finding
    { findingRule = ruleId rule,
      findingScope =
        ScopeOfDecl (generatorFactModuleRef g) (generatorFactDecl g),
      findingSpan = generatorFactSpan g,
      findingMessage = "A GenValid instance outside a -gen package."
    }
