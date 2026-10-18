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

-- | A field holding a scalar the project has already given a type.
--
-- The lexicon this reads is the project's own newtypes rather than a list of
-- words kept here. That is what makes every finding actionable without the rule
-- having a view on which scalars are domain scalars: the author has already
-- decided this one deserves a type and written the type down, so what is left
-- to say is that one site went without it, and which type to use instead.
--
-- It also scales the way the standard does. A project that has named nothing
-- gets nothing, and a project that has named ninety-nine scalars is held to
-- them. That is the rule's honest limit as well: it cannot start the practice,
-- only keep it.
module Hopinion.Check.Project.ScalarNewtypeUnused.Rule (rule) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Database.Esqueleto.Experimental
import Database.Persist.TH
import Hopinion.Check.Project.ScalarNewtypeUnused.Fact
import Hopinion.Compiled (CompiledModules)
import Hopinion.Facts.Module
import Hopinion.Facts.Name
import Hopinion.Facts.Place
import Hopinion.Facts.Primitive (Primitive, unPrimitive)
import Hopinion.Rule
import Hopinion.Rule.Id
import Hopinion.Store (dependencyEdges, packageNames)

-- | The rule's two tables.
--
-- Two rather than one, because the type and the site that went without it are
-- rarely in one module and never necessarily in one package: what the check
-- does is the join between them.
--
-- No keys beyond the row's own. A fact is a row, two modules naming one scalar
-- are two rows saying the same thing, and the lexicon reads them as one name.
share
  [mkPersist sqlSettings, mkMigrate "scalarNewtypeMigration"]
  [persistLowerCase|
ProjectScalar
    package PackageName
    typeHead TypeHead
    primitive Primitive
    deriving Show Eq

ScalarSite
    package PackageName
    component ComponentName
    moduleRef ModuleRef
    owner TypeHead
    name Text
    primitive Primitive
    span Span
    deriving Show Eq
|]

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsScalarNewtypeUnused",
      ruleText = "A field named after a project scalar newtype has that newtype's type.",
      ruleWhy =
        "The project has already decided that this scalar deserves a type and\
        \ written the type down, so a field that spells the type's name and\
        \ keeps the primitive is a site that went without what is already\
        \ there. The name on the field tells a reader what the type would have\
        \ told the compiler: two fields of one primitive can be passed in each\
        \ other's place, which is what the newtype rules out and what a name\
        \ cannot. Where the scalar has genuinely stopped being the domain type,\
        \ which is what a rendering boundary is, say that in a suppression.",
      ruleImpl =
        RuleImplProject
          ProjectCheck
            { projectCheckMigration = scalarNewtypeMigration,
              projectCheckCarry = carry,
              projectCheckFindings = findings
            }
    }

-- | Both halves of one module, which is every scalar it names and every field
-- it writes as a bare primitive.
--
-- The package is recorded on both sides, and the component on the site, because
-- a type is only the fix for a field that can reach it: a package that does not
-- build against the one holding the newtype cannot use it without a cabal file
-- changing too, and that is a different change from the one this rule asks for.
carry :: PackageName -> ModuleContext -> Carry
carry pkg ctx = do
  mapM_
    ( \sn ->
        insert_
          ProjectScalar
            { projectScalarPackage = pkg,
              projectScalarTypeHead = scalarNewtypeType sn,
              projectScalarPrimitive = scalarNewtypePrimitive sn
            }
    )
    (moduleContextScalarNewtypes ctx)
  mapM_
    ( \pf ->
        insert_
          ScalarSite
            { scalarSitePackage = pkg,
              scalarSiteComponent = moduleContextComponentName ctx,
              scalarSiteModuleRef = moduleContextRef ctx,
              scalarSiteOwner = primitiveFieldOwner pf,
              scalarSiteName = primitiveFieldName pf,
              scalarSitePrimitive = primitiveFieldPrimitive pf,
              scalarSiteSpan = primitiveFieldSpan pf
            }
    )
    (moduleContextPrimitiveFields ctx)

-- | Every field the project has a type for and did not use.
--
-- Nothing is put to the compiler. This rule concludes from what is written
-- rather than from what is absent, so a field or a newtype that a splice
-- generates is one it says nothing about rather than one it gets wrong.
findings :: CompiledModules -> Query CheckResult
findings _ = do
  lexicon <- projectScalars
  sites <- scalarSites
  inProject <- packageNames
  edges <- dependencyEdges
  -- The lexicon a site may draw on is the same for every site in one component,
  -- and a repository has a few dozen components against a few thousand fields,
  -- so it is worked out per component rather than per field.
  let lexiconFor :: (PackageName, ComponentName) -> [ScalarNewtype]
      lexiconFor (pkg, component) =
        let reachable = reachablePackages inProject edges pkg component
         in [l | (owner, l) <- lexicon, S.member owner reachable]
  let lexicons :: M.Map (PackageName, ComponentName) [ScalarNewtype]
      lexicons =
        M.fromList
          [ (key, lexiconFor key)
          | key <- S.toList (S.fromList [(pkg, component) | (_, pkg, component, _) <- sites])
          ]
  pure
    ( findingsResult
        [ findingFor ref sn pf
        | (ref, pkg, component, pf) <- sites,
          Just within <- [M.lookup (pkg, component) lexicons],
          Just sn <- [scalarFor within pf]
        ]
    )

-- | The project's lexicon, with the package each name was declared in, which
-- is what decides whether a given site could use it.
--
-- Ordered, because 'scalarFor' leaves the earlier of two names of one length
-- answering and every run has to agree about which one that is.
projectScalars :: Query [(PackageName, ScalarNewtype)]
projectScalars =
  map
    ( \(Value pkg, Value th, Value prim) ->
        ( pkg,
          ScalarNewtype
            { scalarNewtypeType = th,
              scalarNewtypePrimitive = prim
            }
        )
    )
    <$> select
      ( do
          s <- from (table @ProjectScalar)
          orderBy [asc (s ^. ProjectScalarTypeHead), asc (s ^. ProjectScalarId)]
          pure (s ^. ProjectScalarPackage, s ^. ProjectScalarTypeHead, s ^. ProjectScalarPrimitive)
      )

-- | Every field written as a bare primitive, with the module it is in.
--
-- Ordered, because the order rows come back in is the order findings are
-- reported in.
scalarSites :: Query [(ModuleRef, PackageName, ComponentName, PrimitiveField)]
scalarSites =
  map
    ( \(Value ref, Value pkg, Value component, Value owner, Value name, Value prim, Value sp) ->
        ( ref,
          pkg,
          component,
          PrimitiveField
            { primitiveFieldOwner = owner,
              primitiveFieldName = name,
              primitiveFieldPrimitive = prim,
              primitiveFieldSpan = sp
            }
        )
    )
    <$> select
      ( do
          s <- from (table @ScalarSite)
          orderBy [asc (s ^. ScalarSiteModuleRef), asc (s ^. ScalarSiteId)]
          pure
            ( s ^. ScalarSiteModuleRef,
              s ^. ScalarSitePackage,
              s ^. ScalarSiteComponent,
              s ^. ScalarSiteOwner,
              s ^. ScalarSiteName,
              s ^. ScalarSitePrimitive,
              s ^. ScalarSiteSpan
            )
      )

-- | The scope is the declaration the field belongs to, which is the type's own
-- declaration: a field is written inside it, and a suppression a reader puts
-- above the field attaches to it.
findingFor :: ModuleRef -> ScalarNewtype -> PrimitiveField -> Finding
findingFor ref sn pf =
  Finding
    { findingRule = ruleId rule,
      findingScope = ScopeKeyOfDecl ref (DeclName (unTypeHead (primitiveFieldOwner pf))),
      findingSpan = primitiveFieldSpan pf,
      findingMessage =
        T.concat
          [ primitiveFieldName pf,
            " holds a bare ",
            unPrimitive (primitiveFieldPrimitive pf),
            ", and the project has ",
            unTypeHead (scalarNewtypeType sn),
            "."
          ]
    }
