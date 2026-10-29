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

module Hopinion.Check.Package.AppOnlyMain.Rule (rule, StrayAppDeclFact (..)) where

import qualified Data.Text as T
import Database.Esqueleto.Experimental
import Database.Persist.TH
import Hopinion.Check.Package.AppOnlyMain.Fact
import Hopinion.Compiled (CompiledModules)
import Hopinion.Facts.Component
import Hopinion.Facts.Module
import Hopinion.Facts.Name
import Hopinion.Facts.Place
import Hopinion.Rule
import Hopinion.Rule.Id

-- | This rule's table, defined here because this rule is the only thing that
-- writes it and the only thing that reads it.
--
-- One row per declaration rather than one per module, because each is its own
-- thing to move and therefore its own thing to suppress.
-- A module that is not the executable's main module is one row of its own,
-- with no declaration, instead of a row per declaration in it: the whole
-- module is the one thing to move.
--
-- The kind is a column rather than a filter at write time, so that what the
-- store holds is what the module said and the rule's question is asked in one
-- place.
share
  [mkPersist sqlSettings, mkMigrate "strayAppDeclMigration"]
  [persistLowerCase|
StrayAppDeclFact
    package PackageName
    moduleRef ModuleRef
    kind ComponentKind
    span Span
    decl DeclName Maybe
    deriving Show Eq
|]

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsAppOnlyMain",
      ruleText = "An executable's own source is one Main module holding only main = theRealMain.",
      ruleWhy =
        "An executable's modules are private to it, so a test suite can reach\
        \ only the library it calls into. One line naming a function in the\
        \ library moves the code where a test can reach it.",
      ruleImpl =
        RuleImplPackage
          PackageCheck
            { packageCheckMigration = strayAppDeclMigration,
              packageCheckCarry = carry,
              packageCheckFindings = findings
            }
    }

-- | Every main-is module is called @Main@ whatever its file is called, so the
-- name is what tells it apart from the other modules of its component.
carry :: PackageName -> ModuleContext -> Carry
carry pkg ctx =
  let row :: Span -> Maybe DeclName -> StrayAppDeclFact
      row sp decl =
        StrayAppDeclFact
          { strayAppDeclFactPackage = pkg,
            strayAppDeclFactModuleRef = moduleContextRef ctx,
            strayAppDeclFactKind = moduleContextComponent ctx,
            strayAppDeclFactSpan = sp,
            strayAppDeclFactDecl = decl
          }
   in mapM_ insert_ $
        if moduleContextModule ctx == ModuleKey "Main"
          then
            [ row (strayAppDeclSpan d) (Just (strayAppDeclName d))
            | d <- moduleContextStrayAppDecls ctx
            ]
          else [row (wholeFileSpan (moduleContextPath ctx)) Nothing]

-- | Everything this package's executables hold beyond their one line.
--
-- Nothing here asks the compiler anything: a declaration in the wrong place is
-- one that is written down, and where it is written is all of this.
findings :: PackageName -> CompiledModules -> Query CheckResult
findings pkg _ = do
  strays <-
    select
      ( do
          s <- from (table @StrayAppDeclFact)
          where_ (s ^. StrayAppDeclFactPackage ==. val pkg)
          where_ (s ^. StrayAppDeclFactKind ==. val ComponentKindApp)
          orderBy [asc (s ^. StrayAppDeclFactModuleRef), asc (s ^. StrayAppDeclFactId)]
          pure s
      )
  pure (findingsResult (map (findingFor . entityVal) strays))

-- | Which of the three things went wrong is read off the declaration, because
-- @main@ is the one declaration that is allowed to be there and therefore the
-- one whose presence is not the complaint.
findingFor :: StrayAppDeclFact -> Finding
findingFor s =
  let ref = strayAppDeclFactModuleRef s
   in Finding
        { findingRule = ruleId rule,
          findingScope = maybe (ScopeKeyOfFile ref) (ScopeKeyOfDecl ref) (strayAppDeclFactDecl s),
          findingSpan = strayAppDeclFactSpan s,
          findingMessage = case strayAppDeclFactDecl s of
            Nothing ->
              T.concat
                [ "A module beside Main: ",
                  unModuleKey (moduleRefModule ref),
                  "."
                ]
            Just (DeclName "main") -> "This main does more than name the real main."
            Just d ->
              T.concat
                [ "A declaration beside main: ",
                  unDeclName d,
                  "."
                ]
        }
