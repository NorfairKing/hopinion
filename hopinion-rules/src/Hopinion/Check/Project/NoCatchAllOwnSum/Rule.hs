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

module Hopinion.Check.Project.NoCatchAllOwnSum.Rule (rule) where

import Control.Monad.IO.Class (liftIO)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Esqueleto.Experimental
import Database.Persist.TH
import GHC.Hs (GhcPs, LHsExpr, MatchGroup)
import Hopinion.Check.Project.NoCatchAllOwnSum.Extract
import Hopinion.Check.Project.NoCatchAllOwnSum.Fact
import Hopinion.Compiled (CompiledModules, compiledModuleOf)
import Hopinion.Facts.Module
import Hopinion.Facts.Name
import Hopinion.Facts.Place
import Hopinion.Hie (AppliedType (..), SourceRange (..), TypeConstructor (..), compiledModuleConstructorNamedAt, compiledModuleTypeAt)
import Hopinion.Parse (ParsedModule)
import Hopinion.Rule
import Hopinion.Rule.Id
import Hopinion.Rule.Prose (proseBeside)
import Hopinion.Store

-- | This rule's tables, defined here because this rule is the only thing that
-- writes them and the only thing that reads them.
--
-- Apart, because a catch-all and the type it matches on are usually in
-- different modules, and often in different packages.
-- Which type a catch-all matches on, and which type a synonym or a data family
-- instance names, is answered from a module's @.hie@ file, which the module
-- phase does not have.
--
-- An argument of a data family instance is found by the family's span rather
-- than by a row id, because merging the packages' stores renumbers rows.
share
  [mkPersist sqlSettings, mkMigrate "noCatchAllOwnSumMigration"]
  [persistLowerCase|
CatchAllSite
    moduleRef ModuleRef
    file StoredPath
    span Span
    decl DeclName Maybe
    deriving Show Eq

OwnSumType
    defining ModuleKey
    name Text
    deriving Show Eq

OwnSynonym
    defining ModuleKey
    name Text
    file StoredPath
    head Span
    deriving Show Eq

OwnDataInstance
    file StoredPath
    family Span
    deriving Show Eq

OwnDataInstanceArgument
    family Span
    position Int
    head Span Maybe
    deriving Show Eq
|]

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsNoCatchAllOwnSum",
      ruleText = "Match on the project's own sum types by constructor, not with a catch-all.",
      ruleWhy = $(proseBeside "why.txt"),
      ruleFix = $(proseBeside "fix.txt"),
      ruleImpl =
        RuleImplProject
          ProjectCheck
            { projectCheckMigration = noCatchAllOwnSumMigration,
              projectCheckCarry = ModuleCarryOverNodes carry,
              projectCheckFindings = findings
            }
    }

-- | Every catch-all, whatever it matches on, and every sum type, wherever it
-- is used: which of the one is on which of the other takes the compiler.
carry :: PackageName -> ModuleContext -> ParsedModule -> [MatchGroup GhcPs (LHsExpr GhcPs)] -> Carry
carry _ ctx parsed groups = do
  let file = StoredPath (moduleContextPath ctx)
  mapM_
    insert_
    [ CatchAllSite
        { catchAllSiteModuleRef = moduleContextRef ctx,
          catchAllSiteFile = file,
          catchAllSiteSpan = catchAllSpan c,
          catchAllSiteDecl = catchAllDecl c
        }
    | c <- catchAllsOf ctx groups
    ]
  mapM_
    insert_
    [ OwnSumType
        { ownSumTypeDefining = moduleContextModule ctx,
          ownSumTypeName = unSumType t
        }
    | t <- sumTypesOf parsed
    ]
  mapM_
    insert_
    [ OwnSynonym
        { ownSynonymDefining = moduleContextModule ctx,
          ownSynonymName = synonymName s,
          ownSynonymFile = file,
          ownSynonymHead = synonymHead s
        }
    | s <- synonymsOf ctx parsed
    ]
  mapM_
    ( \i -> do
        insert_
          OwnDataInstance
            { ownDataInstanceFile = file,
              ownDataInstanceFamily = dataInstanceFamily i
            }
        mapM_
          insert_
          [ OwnDataInstanceArgument
              { ownDataInstanceArgumentFamily = dataInstanceFamily i,
                ownDataInstanceArgumentPosition = position,
                ownDataInstanceArgumentHead = h
              }
          | (position, h) <- zip [0 ..] (dataInstanceArguments i)
          ]
    )
    (dataInstancesOf ctx parsed)

-- | Every catch-all the compiler says matches on a sum type of the project.
--
-- A type is told apart by its name and the name of the module that defines it.
-- A @.hie@ file also names the unit, but what the carry knows of a module is
-- its name, so the unit is the one thing the two sides cannot agree on.
--
-- No @.hie@ file is read when the project declares nothing a catch-all could be
-- over, since reading one costs more than parsing its module did.
findings :: CompiledModules -> Query CheckResult
findings compiled = do
  sums <- map entityVal <$> select (from (table @OwnSumType))
  synonyms <- map entityVal <$> select (from (table @OwnSynonym))
  instances <- map entityVal <$> select (from (table @OwnDataInstance))
  if null sums && null synonyms && null instances
    then pure (findingsResult [])
    else do
      sites <- map entityVal <$> select (from (table @CatchAllSite))
      arguments <- map entityVal <$> select (from (table @OwnDataInstanceArgument))
      let namedAt :: StoredPath -> Span -> IO (Maybe TypeConstructor)
          namedAt file sp = do
            c <- compiledModuleOf (unStoredPath file) compiled
            pure (compiledModuleConstructorNamedAt c (rangeOf sp))
      synonymHeads <-
        liftIO
          ( traverse
              ( \s -> do
                  h <- namedAt (ownSynonymFile s) (ownSynonymHead s)
                  pure ((ownSynonymDefining s, ownSynonymName s), keyOf <$> h)
              )
              synonyms
          )
      ownInstances <-
        liftIO
          ( traverse
              ( \i -> do
                  let file = ownDataInstanceFile i
                  family <- namedAt file (ownDataInstanceFamily i)
                  args <-
                    traverse
                      (maybe (pure Nothing) (namedAt file) . ownDataInstanceArgumentHead)
                      (sortOn ownDataInstanceArgumentPosition [a | a <- arguments, ownDataInstanceArgumentFamily a == ownDataInstanceFamily i])
                  pure ((,) <$> family <*> pure args)
              )
              instances
          )
      let owned = ownedThroughSynonyms (M.fromList [((ownSumTypeDefining t, ownSumTypeName t), ownSumTypeName t) | t <- sums]) synonymHeads
      -- The name of the sum type a catch-all is over, which a synonym is not.
      let ownSumOf :: AppliedType -> Maybe Text
          ownSumOf th =
            let tc = appliedTypeConstructor th
             in case M.lookup (keyOf tc) owned of
                  Just name -> Just name
                  Nothing
                    | any (instanceOf th) (catMaybes ownInstances) -> Just (typeConstructorName tc)
                    | otherwise -> Nothing
      typed <-
        liftIO
          ( traverse
              ( \s -> do
                  c <- compiledModuleOf (unStoredPath (catchAllSiteFile s)) compiled
                  pure (s, compiledModuleTypeAt c (rangeOf (catchAllSiteSpan s)))
              )
              sites
          )
      pure
        ( findingsResult
            [ findingFor s name
            | (s, Just th) <- typed,
              Just name <- [ownSumOf th]
            ]
        )

keyOf :: TypeConstructor -> (ModuleKey, Text)
keyOf tc = (ModuleKey (typeConstructorModule tc), typeConstructorName tc)

-- | The sum types by name, and every synonym whose head is one of them or is
-- another such synonym, by the name of the sum type it stands for.
--
-- A @.hie@ file writes a type the way some code in its module first wrote it,
-- so a module that mentions a synonym of a sum type can have the synonym as the
-- type of every pattern on that sum.
ownedThroughSynonyms :: Map (ModuleKey, Text) Text -> [((ModuleKey, Text), Maybe (ModuleKey, Text))] -> Map (ModuleKey, Text) Text
ownedThroughSynonyms owned synonyms =
  let added =
        M.fromList
          [ (s, name)
          | (s, Just h) <- synonyms,
            M.notMember s owned,
            Just name <- [M.lookup h owned]
          ]
   in if M.null added
        then owned
        else ownedThroughSynonyms (M.union owned added) synonyms

-- | Whether a type is this instance of a data family: the family at its head,
-- applied to arguments with the heads the instance wrote.
--
-- An argument the instance wrote no constructor for, such as a type variable,
-- accepts any.
instanceOf :: AppliedType -> (TypeConstructor, [Maybe TypeConstructor]) -> Bool
instanceOf th (family, args) =
  appliedTypeConstructor th == family
    && length args == length (appliedTypeArguments th)
    && and (zipWith (\written actual -> maybe True (\w -> actual == Just w) written) args (appliedTypeArguments th))

rangeOf :: Span -> SourceRange
rangeOf sp =
  SourceRange
    { sourceRangeStartLine = fromIntegral (positionLine (spanStart sp)),
      sourceRangeStartColumn = fromIntegral (positionCol (spanStart sp)),
      sourceRangeEndLine = fromIntegral (positionLine (spanEnd sp)),
      sourceRangeEndColumn = fromIntegral (positionCol (spanEnd sp))
    }

findingFor :: CatchAllSite -> Text -> Finding
findingFor s typeName =
  let ref = catchAllSiteModuleRef s
   in Finding
        { findingRule = ruleId rule,
          findingScope = maybe (ScopeKeyOfFile ref) (ScopeKeyOfDecl ref) (catchAllSiteDecl s),
          findingSpan = catchAllSiteSpan s,
          findingMessage = T.concat ["A catch-all over ", typeName, ", which this project defines."]
        }
