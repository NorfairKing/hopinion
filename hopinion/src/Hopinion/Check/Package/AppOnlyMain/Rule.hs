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
    decl DeclName
    deriving Show Eq
|]

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsAppOnlyMain",
      ruleText = "An executable's own source holds only main = theRealMain.",
      ruleWhy =
        "Only a library can be imported, so only a library can be tested. One\
        \ line naming a function in the library moves the code where a test can\
        \ reach it.",
      ruleImpl =
        PackageRule
          PackageCheck
            { packageCheckMigration = strayAppDeclMigration,
              packageCheckCarry = carry,
              packageCheckFindings = findings
            }
    }

carry :: PackageName -> ModuleContext -> Carry
carry pkg ctx =
  mapM_
    insert_
    [ StrayAppDeclFact
        { strayAppDeclFactPackage = pkg,
          strayAppDeclFactModuleRef = moduleContextRef ctx,
          strayAppDeclFactKind = moduleContextComponent ctx,
          strayAppDeclFactSpan = strayAppDeclSpan d,
          strayAppDeclFactDecl = strayAppDeclName d
        }
    | d <- moduleContextStrayAppDecls ctx
    ]

-- | Everything this package's executables declare beyond their one line.
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
          where_ (s ^. StrayAppDeclFactKind ==. val ComponentApp)
          orderBy [asc (s ^. StrayAppDeclFactModuleRef), asc (s ^. StrayAppDeclFactId)]
          pure s
      )
  pure (findingsResult (map (findingFor . entityVal) strays))

-- | Which of the two things went wrong is read off the name, because @main@ is
-- the one declaration that is allowed to be there and therefore the one whose
-- presence is not the complaint.
findingFor :: StrayAppDeclFact -> Finding
findingFor s =
  Finding
    { findingRule = ruleId rule,
      findingScope = ScopeOfDecl (strayAppDeclFactModuleRef s) (strayAppDeclFactDecl s),
      findingSpan = strayAppDeclFactSpan s,
      findingMessage =
        if strayAppDeclFactDecl s == DeclName "main"
          then "This main does the work itself."
          else
            T.concat
              [ "A declaration beside main: ",
                declNameText (strayAppDeclFactDecl s),
                "."
              ]
    }
