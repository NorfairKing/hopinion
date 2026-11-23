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

module Hopinion.Check.Package.NoRestatedValidityDefaults.Rule (rule) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Esqueleto.Experimental
import Database.Persist.TH
import Hopinion.Check.Package.NoRestatedValidityDefaults.Extract
import Hopinion.Check.Package.NoRestatedValidityDefaults.Fact
import Hopinion.Compiled (CompiledModules, declaredInstancesOf)
import Hopinion.Extract (instanceDeclName)
import Hopinion.Facts.Instance
import Hopinion.Facts.Module
import Hopinion.Facts.Name
import Hopinion.Facts.Place
import Hopinion.Hie (DeclaredInstance (..))
import Hopinion.Rule
import Hopinion.Rule.Id
import Hopinion.Rule.Prose (proseBeside)
import Hopinion.Store

-- | This rule's table, defined here because this rule is the only thing that
-- writes it and the only thing that reads it.
--
-- One row per method that restates the default of a class by that name.
-- Whether the class is the one with that default is answered from the
-- module's interface, which the module phase does not have.
share
  [mkPersist sqlSettings, mkMigrate "restatedDefaultMigration"]
  [persistLowerCase|
RestatedDefault
    package PackageName
    moduleRef ModuleRef
    file StoredPath
    span Span
    className Text
    classModule Text
    typeHead TypeHead
    method Text
    deriving Show Eq
|]

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsNoRestatedValidityDefaults",
      ruleText = "Validity and GenValid instances leave out methods that restate a default.",
      ruleWhy = $(proseBeside "why.txt"),
      ruleFix = $(proseBeside "fix.txt"),
      ruleImpl =
        RuleImplPackage
          PackageCheck
            { packageCheckMigration = restatedDefaultMigration,
              packageCheckCarry = ModuleCarryFromContext carry,
              packageCheckFindings = findings
            }
    }

carry :: PackageName -> ModuleContext -> Carry
carry pkg ctx = case moduleContextSyntax ctx of
  ModuleSyntaxParsed parsed _ ->
    mapM_
      (insert_ . rowFor)
      (restatedMethodsOf ctx parsed)
  ModuleSyntaxFailed _ _ -> pure ()
  ModuleSyntaxNotHaskellSource -> pure ()
  where
    rowFor :: RestatedMethod -> RestatedDefault
    rowFor rm =
      let inst = restatedMethodInstance rm
       in RestatedDefault
            { restatedDefaultPackage = pkg,
              restatedDefaultModuleRef = moduleContextRef ctx,
              restatedDefaultFile = StoredPath (moduleContextPath ctx),
              restatedDefaultSpan = restatedMethodSpan rm,
              restatedDefaultClassName = instanceFactClass inst,
              restatedDefaultClassModule = restatedMethodClassModule rm,
              restatedDefaultTypeHead = instanceFactType inst,
              restatedDefaultMethod = restatedMethodName rm
            }

-- | Every restated method whose instance the compiler says is of the class
-- with that default, rather than of another class by the same name.
findings :: PackageName -> CompiledModules -> Query CheckResult
findings pkg compiled = do
  rows <-
    select
      ( do
          r <- from (table @RestatedDefault)
          where_ (r ^. RestatedDefaultPackage ==. val pkg)
          orderBy [asc (r ^. RestatedDefaultModuleRef), asc (r ^. RestatedDefaultId)]
          pure r
      )
  confirmed <- traverse (confirm . entityVal) rows
  pure (findingsResult [findingFor r | (r, True) <- confirmed])
  where
    confirm :: RestatedDefault -> Query (RestatedDefault, Bool)
    confirm r = do
      instances <- liftIO (declaredInstancesOf (unStoredPath (restatedDefaultFile r)) compiled)
      pure (r, any (isInstanceOf r) instances)

    isInstanceOf :: RestatedDefault -> DeclaredInstance -> Bool
    isInstanceOf r i =
      declaredInstanceClass i == restatedDefaultClassName r
        && declaredInstanceClassModule i == restatedDefaultClassModule r
        && TypeHead (declaredInstanceType i) == restatedDefaultTypeHead r

findingFor :: RestatedDefault -> Finding
findingFor r =
  Finding
    { findingRule = ruleId rule,
      findingScope =
        ScopeKeyOfDecl
          (restatedDefaultModuleRef r)
          (instanceDeclName (restatedDefaultClassName r) (restatedDefaultTypeHead r)),
      findingSpan = restatedDefaultSpan r,
      findingMessage =
        T.concat
          [ restatedDefaultMethod r,
            " in ",
            restatedDefaultClassName r,
            " ",
            unTypeHead (restatedDefaultTypeHead r),
            " restates its default."
          ]
    }
