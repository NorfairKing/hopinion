{-# LANGUAGE OverloadedStrings #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks whether an expression does work.
module Hopinion.Check.Package.AppOnlyMain.Extract (strayAppDeclsOf) where

import GHC.Hs
import GHC.Types.SrcLoc (unLoc)
import Hopinion.Check.Package.AppOnlyMain.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Decl
import Hopinion.Facts.Name
import Path (File, Path, Rel)

-- | Everything this module declares that an executable may not.
--
-- Read for every module rather than only for an executable's, because which
-- component claims a module is the rule's question rather than extraction's.
--
-- The declaration list is what says what is there, so a declaration whose name
-- this does not have to work out is one it cannot get wrong. Only @main@ is
-- read out of the parse tree, and only for what its right-hand side does.
--
-- A type signature is not one of these. It cannot be moved on its own, and the
-- binding it belongs to is reported already, so counting it would ask for two
-- suppressions where there is one thing to move.
strayAppDeclsOf :: Path Rel File -> [DeclFact] -> [LHsDecl GhcPs] -> [StrayAppDecl]
strayAppDeclsOf rp decls ds =
  [ StrayAppDecl
      { strayAppDeclName = declFactName d,
        strayAppDeclSpan = declFactSpan d
      }
  | d <- decls,
    declFactKind d /= DeclKindSignature,
    declFactName d /= mainName
  ]
    ++ [ StrayAppDecl
           { strayAppDeclName = mainName,
             strayAppDeclSpan = spanOfSrcSpan rp (getLocA ldecl)
           }
       | ldecl <- ds,
         ValD _ FunBind {fun_id = n, fun_matches = mg} <- [unLoc ldecl],
         rdrText (unLoc n) == "main",
         bindingDoesWork mg
       ]

mainName :: DeclName
mainName = DeclName "main"

-- | Whether this binding does anything beyond naming what to run.
--
-- Guards, a where clause and more than one equation all count, since each is a
-- decision written here rather than in something a test can reach. So does an
-- argument: @main@ takes none, and a binding that takes one is not the @main@
-- this is about.
bindingDoesWork :: MatchGroup GhcPs (LHsExpr GhcPs) -> Bool
bindingDoesWork mg = case unLoc (mg_alts mg) of
  [lm] ->
    let m = unLoc lm
     in case (unLoc (m_pats m), m_grhss m) of
          ([], GRHSs {grhssGRHSs = [lgrhs], grhssLocalBinds = EmptyLocalBinds _}) ->
            case unLoc lgrhs of
              GRHS _ [] body -> not (isBareName body)
              GRHS {} -> True
          _ -> True
  _ -> True

-- | Whether this expression is one name and nothing else.
--
-- Arguments and operators count as work too, not only control flow: a literal
-- passed to the real main is a decision a test cannot reach, and @a >> b@ is a
-- sequence a test cannot reach.
-- Parentheses around the name do not, because they change nothing.
isBareName :: LHsExpr GhcPs -> Bool
isBareName body = case unLoc body of
  HsVar {} -> True
  HsPar _ inner -> isBareName inner
  _ -> False
