{-# LANGUAGE LambdaCase #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks which alternatives match anything at all.
module Hopinion.Check.Project.NoCatchAllOwnSum.Extract
  ( catchAllsOf,
    sumTypesOf,
    synonymsOf,
    dataInstancesOf,
  )
where

import Data.List (transpose)
import GHC.Hs
import GHC.Types.SrcLoc (unLoc)
import Hopinion.Check.Project.NoCatchAllOwnSum.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Module
import Hopinion.Facts.Place
import Hopinion.Parse (ParsedModule, parsedModuleDecls)

-- | Every catch-all among the alternatives of a @case@, a @\\case@, a
-- @\\cases@ or a function's equations, wherever it is written.
--
-- Argument by argument, since each of a function's arguments is matched on by
-- its own column of patterns.
-- A column is matched on by constructor when one of its patterns is a
-- constructor pattern, and in such a column every pattern that is a wildcard
-- or a variable matches whatever the others leave over.
-- A column with no constructor pattern takes nothing apart, which is also what
-- makes a lone alternative never a fallback.
--
-- An alternative that can fall through, because of a guard or because of
-- another of its patterns, still counts: whenever it is taken, its catch-all
-- takes any constructor added later without the compiler saying so.
--
-- The top-level pattern only, with parentheses, bangs, laziness, signatures and
-- as-patterns seen through: a catch-all nested inside a constructor pattern
-- matches a field, and is about the field's type rather than this one.
catchAllsOf :: ModuleContext -> [MatchGroup GhcPs (LHsExpr GhcPs)] -> [CatchAll]
catchAllsOf ctx groups =
  let rp = moduleContextPath ctx
      ref = moduleContextRef ctx
      decls = moduleContextDecls ctx
   in [ CatchAll
          { catchAllSpan = sp,
            catchAllDecl = case declScopeOf ref decls sp of
              ScopeKeyOfDecl _ d -> Just d
              ScopeKeyOfFile _ -> Nothing
          }
      | mg <- groups,
        column <-
          transpose
            [ unLoc (m_pats (unLoc lm))
            | lm <- unLoc (mg_alts mg)
            ],
        any (isConstructorPattern . unLoc) column,
        lp <- column,
        Just wild <- [catchAllPattern lp],
        let sp = spanOfSrcSpan rp (getLocA wild)
      ]

-- | The wildcard or variable this pattern is, under anything that does not
-- change what it matches.
catchAllPattern :: LPat GhcPs -> Maybe (LPat GhcPs)
catchAllPattern lp = case unLoc lp of
  WildPat _ -> Just lp
  VarPat _ _ -> Just lp
  ParPat _ p -> catchAllPattern p
  BangPat _ p -> catchAllPattern p
  LazyPat _ p -> catchAllPattern p
  SigPat _ p _ -> catchAllPattern p
  AsPat _ _ p -> catchAllPattern p
  _ -> Nothing

isConstructorPattern :: Pat GhcPs -> Bool
isConstructorPattern = \case
  ConPat {} -> True
  ParPat _ p -> isConstructorPattern (unLoc p)
  BangPat _ p -> isConstructorPattern (unLoc p)
  LazyPat _ p -> isConstructorPattern (unLoc p)
  SigPat _ p _ -> isConstructorPattern (unLoc p)
  AsPat _ _ p -> isConstructorPattern (unLoc p)
  _ -> False

-- | Every data type this module declares with more than one constructor.
sumTypesOf :: ParsedModule -> [SumType]
sumTypesOf parsed =
  [ SumType (rdrText (unLoc (tcdLName decl)))
  | ldecl <- parsedModuleDecls parsed,
    TyClD _ decl@DataDecl {} <- [unLoc ldecl],
    isSum (tcdDataDefn decl)
  ]

-- | Every type synonym this module declares whose right-hand side is headed by
-- a named type, which might be one of the project's sum types.
--
-- Which type that name is takes the compiler, so this is only where it is
-- written.
synonymsOf :: ModuleContext -> ParsedModule -> [Synonym]
synonymsOf ctx parsed =
  [ Synonym
      { synonymName = rdrText (unLoc n),
        synonymHead = spanOfSrcSpan (moduleContextPath ctx) (getLocA headName)
      }
  | ldecl <- parsedModuleDecls parsed,
    TyClD _ SynDecl {tcdLName = n, tcdRhs = rhs} <- [unLoc ldecl],
    Just headName <- [namedHead rhs]
  ]

-- | Every instance of a data family this module declares with more than one
-- constructor, wherever the family is declared.
--
-- Each instance is its own sum: matching on one with a catch-all hides only
-- the constructors this project wrote, so the instance is told apart by the
-- arguments the family is applied to.
dataInstancesOf :: ModuleContext -> ParsedModule -> [DataInstance]
dataInstancesOf ctx parsed =
  let rp = moduleContextPath ctx
      inClassInstance :: InstDecl GhcPs -> [DataFamInstDecl GhcPs]
      inClassInstance = \case
        ClsInstD _ ci -> map unLoc (cid_datafam_insts ci)
        DataFamInstD _ d -> [d]
        TyFamInstD _ _ -> []
   in [ DataInstance
          { dataInstanceFamily = spanOfSrcSpan rp (getLocA (feqn_tycon eqn)),
            dataInstanceArguments =
              [ spanOfSrcSpan rp . getLocA <$> namedHead t
              | HsValArg _ t <- feqn_pats eqn
              ]
          }
      | ldecl <- parsedModuleDecls parsed,
        InstD _ inst <- [unLoc ldecl],
        DataFamInstDecl eqn <- inClassInstance inst,
        isSum (feqn_rhs eqn)
      ]

isSum :: HsDataDefn GhcPs -> Bool
isSum defn = case dd_cons defn of
  DataTypeCons _ cons -> sum (map (constructorCount . unLoc) cons) > 1
  NewTypeCon _ -> False

-- | A GADT declaration can name several constructors in one signature, and
-- each of those counts.
constructorCount :: ConDecl GhcPs -> Int
constructorCount = \case
  ConDeclH98 {} -> 1
  ConDeclGADT {con_names = names} -> length names
