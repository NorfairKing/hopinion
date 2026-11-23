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

module Hopinion.Check.Project.GenValidWithoutExtraChecking.Rule (rule) where

import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Database.Esqueleto.Experimental
import Database.Persist.TH
import Hopinion.Check.Project.GenValidWithoutExtraChecking.Extract
import Hopinion.Check.Project.GenValidWithoutExtraChecking.Fact
import Hopinion.Compiled (CompiledModules, compiledModuleOf, sourceRangeOfSpan)
import Hopinion.Facts.Instance
import Hopinion.Facts.Module
import Hopinion.Facts.Name
import Hopinion.Facts.Place
import Hopinion.Hie (TypeConstructor (..), compiledModuleConstructorNamedAt)
import Hopinion.Rule
import Hopinion.Rule.Id
import Hopinion.Rule.Prose (proseBeside)
import Hopinion.Store

-- | This rule's tables, defined here because this rule is the only thing that
-- writes them and the only thing that reads them.
--
-- Apart, because a GenValid instance belongs in a gen package and the
-- Validity instance of its type does not.
-- Which class and which type an instance names is answered from the module's
-- @.hie@ file, which the module phase does not have.
share
  [mkPersist sqlSettings, mkMigrate "genValidWithoutExtraCheckingMigration"]
  [persistLowerCase|
StructuralValidity
    file StoredPath
    classAt Span
    typeAt Span
    deriving Show Eq

DefaultingGenValidInstance
    moduleRef ModuleRef
    file StoredPath
    span Span
    decl DeclName Maybe
    classAt Span
    typeAt Span
    typeHead TypeHead
    methods [Text]
    definitions [Text]
    deriving Show Eq
|]

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsGenValidWithoutExtraChecking",
      ruleText =
        "GenValid skips the validity check when Validity checks nothing but the fields.",
      ruleWhy = $(proseBeside "why.txt"),
      ruleFix = $(proseBeside "fix.txt"),
      ruleImpl =
        RuleImplProject
          ProjectCheck
            { projectCheckMigration = genValidWithoutExtraCheckingMigration,
              projectCheckCarry = ModuleCarryFromContext carry,
              projectCheckFindings = findings
            }
    }

carry :: PackageName -> ModuleContext -> Carry
carry _ ctx = case moduleContextSyntax ctx of
  ModuleSyntaxParsed parsed _ -> do
    let file = StoredPath (moduleContextPath ctx)
    mapM_
      insert_
      [ StructuralValidity
          { structuralValidityFile = file,
            structuralValidityClassAt = writtenInstanceClassAt w,
            structuralValidityTypeAt = writtenInstanceTypeAt w
          }
      | w <- structuralValiditiesOf ctx parsed
      ]
    mapM_
      insert_
      [ DefaultingGenValidInstance
          { defaultingGenValidInstanceModuleRef = moduleContextRef ctx,
            defaultingGenValidInstanceFile = file,
            defaultingGenValidInstanceSpan = instanceFactSpan inst,
            defaultingGenValidInstanceDecl = case instanceFactScope inst of
              ScopeKeyOfDecl _ d -> Just d
              ScopeKeyOfFile _ -> Nothing,
            defaultingGenValidInstanceClassAt = writtenInstanceClassAt w,
            defaultingGenValidInstanceTypeAt = writtenInstanceTypeAt w,
            defaultingGenValidInstanceTypeHead = instanceFactType inst,
            defaultingGenValidInstanceMethods = map genValidMethodName methods,
            defaultingGenValidInstanceDefinitions =
              [ T.unwords [genValidMethodName m, "=", genValidMethodReplacement m]
              | m <- methods
              ]
          }
      | defaulting <- defaultingGenValidsOf ctx parsed,
        let w = defaultingGenValidInstance defaulting,
        let inst = writtenInstanceFact w,
        let methods = NE.toList (defaultingGenValidMethods defaulting)
      ]
  ModuleSyntaxFailed _ _ -> pure ()
  ModuleSyntaxNotHaskellSource -> pure ()

-- | A type the way the compiler names it: by its name and the module that
-- defines it, which tells it apart from another type by the same name.
data DefinedType = DefinedType
  { definedTypeModule :: !Text,
    definedTypeName :: !Text
  }
  deriving stock (Eq, Ord)

-- | Every defaulted GenValid method whose type the compiler says has a
-- structural Validity instance in this project.
--
-- No @.hie@ file is read when the project has no structural Validity
-- instance, since then nothing can be reported.
findings :: CompiledModules -> Query CheckResult
findings compiled = do
  structurals <- map entityVal <$> select (from (table @StructuralValidity))
  if null structurals
    then pure (findingsResult [])
    else do
      defaulted <-
        map entityVal
          <$> select
            ( do
                d <- from (table @DefaultingGenValidInstance)
                orderBy [asc (d ^. DefaultingGenValidInstanceModuleRef), asc (d ^. DefaultingGenValidInstanceId)]
                pure d
            )
      structuralTypes <-
        liftIO
          ( S.fromList . catMaybes
              <$> traverse
                ( \s ->
                    instanceTypeOf
                      TypeConstructor
                        { typeConstructorModule = "Data.Validity",
                          typeConstructorName = "Validity"
                        }
                      (structuralValidityFile s)
                      (structuralValidityClassAt s)
                      (structuralValidityTypeAt s)
                      compiled
                )
                structurals
          )
      reported <-
        liftIO
          ( filterM
              ( \d ->
                  maybe False (`S.member` structuralTypes)
                    <$> instanceTypeOf
                      TypeConstructor
                        { typeConstructorModule = "Data.GenValidity",
                          typeConstructorName = "GenValid"
                        }
                      (defaultingGenValidInstanceFile d)
                      (defaultingGenValidInstanceClassAt d)
                      (defaultingGenValidInstanceTypeAt d)
                      compiled
              )
              defaulted
          )
      pure (findingsResult (map findingFor reported))

-- | The type an instance is for, when the compiler says the class it names is
-- this one.
--
-- A class is asked the way a type is, because to a @.hie@ file both are type
-- constructors.
-- Asked rather than read off the source, because the source names the class
-- and the type however they were imported, and a project can define a class
-- by the same name.
instanceTypeOf :: TypeConstructor -> StoredPath -> Span -> Span -> CompiledModules -> IO (Maybe DefinedType)
instanceTypeOf cls file classAt typeAt compiled = do
  c <- compiledModuleOf (unStoredPath file) compiled
  pure $ case (compiledModuleConstructorNamedAt c (sourceRangeOfSpan classAt), compiledModuleConstructorNamedAt c (sourceRangeOfSpan typeAt)) of
    (Just named, Just ty)
      | named == cls ->
          Just
            DefinedType
              { definedTypeModule = typeConstructorModule ty,
                definedTypeName = typeConstructorName ty
              }
    _ -> Nothing

findingFor :: DefaultingGenValidInstance -> Finding
findingFor d =
  let ref = defaultingGenValidInstanceModuleRef d
      th = unTypeHead (defaultingGenValidInstanceTypeHead d)
   in Finding
        { findingRule = ruleId rule,
          findingScope = maybe (ScopeKeyOfFile ref) (ScopeKeyOfDecl ref) (defaultingGenValidInstanceDecl d),
          findingSpan = defaultingGenValidInstanceSpan d,
          findingMessage =
            T.concat
              [ "GenValid ",
                th,
                " checks validity again in the default ",
                T.intercalate " and " (defaultingGenValidInstanceMethods d),
                ", but Validity ",
                th,
                " checks only the fields. Define ",
                T.intercalate " and " (defaultingGenValidInstanceDefinitions d),
                "."
              ]
        }
