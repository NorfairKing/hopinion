{-# LANGUAGE DataKinds #-}
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

module Hopinion.Check.Package.ValidateExtendsGeneric.Rule (rule) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Esqueleto.Experimental
import Database.Persist.TH
import Hopinion.Check.Package.ValidateExtendsGeneric.Extract
import Hopinion.Check.Package.ValidateExtendsGeneric.Fact
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
-- One row per @validate@ of another shape in an instance of a class by that
-- name, with what is wrong with it.
-- What is wrong is stored as the words a finding says, because nothing but the
-- finding reads it.
-- Whether the class is the one from validity is answered from the module's
-- interface, which the module phase does not have.
share
  [mkPersist sqlSettings, mkMigrate "unshapedValidateMigration"]
  [persistLowerCase|
UnshapedValidateRow
    package PackageName
    moduleRef ModuleRef
    file StoredPath
    span Span
    className Text
    typeHead TypeHead
    binding Text Maybe
    complaint Text
    deriving Show Eq
|]

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsValidateExtendsGeneric",
      ruleText = "A written-out validate is mconcat [genericValidate x, ...] with more checks.",
      ruleWhy = $(proseBeside "why.txt"),
      ruleFix = $(proseBeside "fix.txt"),
      ruleImpl =
        RuleImplPackage
          PackageCheck
            { packageCheckMigration = unshapedValidateMigration,
              packageCheckCarry = ModuleCarryFromContext carry,
              packageCheckFindings = findings
            }
    }

carry :: PackageName -> ModuleContext -> Carry
carry pkg ctx = case moduleContextSyntax ctx of
  ModuleSyntaxParsed parsed _ ->
    let rowFor :: UnshapedValidate -> UnshapedValidateRow
        rowFor uv =
          UnshapedValidateRow
            { unshapedValidateRowPackage = pkg,
              unshapedValidateRowModuleRef = moduleContextRef ctx,
              unshapedValidateRowFile = StoredPath (moduleContextPath ctx),
              unshapedValidateRowSpan = unshapedValidateSpan uv,
              unshapedValidateRowClassName = instanceFactClass (unshapedValidateInstance uv),
              unshapedValidateRowTypeHead = instanceFactType (unshapedValidateInstance uv),
              unshapedValidateRowBinding = case unshapedValidateSite uv of
                ValidateSiteMethod -> Nothing
                ValidateSiteBinding binding -> Just binding,
              unshapedValidateRowComplaint = complaintOf (unshapedValidateShape uv)
            }
     in mapM_ (insert_ . rowFor) (unshapedValidatesOf ctx parsed)
  ModuleSyntaxFailed _ _ -> pure ()
  ModuleSyntaxNotHaskellSource -> pure ()

-- | Every such @validate@ whose instance the compiler says is of validity's
-- class, rather than of another class by the same name.
findings :: PackageName -> CompiledModules -> Query CheckResult
findings pkg compiled = do
  rows <-
    select
      ( do
          r <- from (table @UnshapedValidateRow)
          where_ (r ^. UnshapedValidateRowPackage ==. val pkg)
          orderBy [asc (r ^. UnshapedValidateRowModuleRef), asc (r ^. UnshapedValidateRowId)]
          pure r
      )
  let isInstanceOf :: UnshapedValidateRow -> DeclaredInstance -> Bool
      isInstanceOf r i =
        declaredInstanceClass i == unshapedValidateRowClassName r
          && declaredInstanceClassModule i == "Data.Validity"
          && TypeHead (declaredInstanceType i) == unshapedValidateRowTypeHead r
  let confirm :: UnshapedValidateRow -> Query (UnshapedValidateRow, Bool)
      confirm r = do
        instances <- liftIO (declaredInstancesOf (unStoredPath (unshapedValidateRowFile r)) compiled)
        pure (r, any (isInstanceOf r) instances)
  confirmed <- traverse (confirm . entityVal) rows
  pure (findingsResult [findingFor r | (r, True) <- confirmed])

findingFor :: UnshapedValidateRow -> Finding
findingFor r =
  Finding
    { findingRule = ruleId rule,
      findingScope =
        ScopeKeyOfDecl
          (unshapedValidateRowModuleRef r)
          ( maybe
              (instanceDeclName (unshapedValidateRowClassName r) (unshapedValidateRowTypeHead r))
              DeclName
              (unshapedValidateRowBinding r)
          ),
      findingSpan = unshapedValidateRowSpan r,
      findingMessage =
        T.concat
          [ case unshapedValidateRowBinding r of
              Nothing -> "validate in "
              Just binding -> T.concat [binding, ", which validate in "],
            unshapedValidateRowClassName r,
            " ",
            unTypeHead (unshapedValidateRowTypeHead r),
            case unshapedValidateRowBinding r of
              Nothing -> " "
              Just _ -> " is, ",
            unshapedValidateRowComplaint r
          ]
    }

complaintOf :: ValidateShape -> Text
complaintOf = \case
  ValidateShapeRestatesDefault -> "restates its default."
  ValidateShapeAlwaysValid -> "calls every value valid without looking at it."
  ValidateShapeBindingApart -> "names a binding that is not next to the instance."
  ValidateShapeOther -> "is not mconcat of genericValidate of its argument followed by more checks."
