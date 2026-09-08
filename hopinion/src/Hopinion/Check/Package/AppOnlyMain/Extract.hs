{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks whether an expression does work.
module Hopinion.Check.Package.AppOnlyMain.Extract (strayAppDeclsOf) where

import Data.Generics (listify)
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
  [ StrayAppDecl {strayAppDeclName = declFactName d, strayAppDeclSpan = declFactSpan d}
  | d <- decls,
    declFactKind d /= DeclSignature,
    declFactName d /= mainName
  ]
    ++ [ StrayAppDecl {strayAppDeclName = mainName, strayAppDeclSpan = spanOfSrcSpan rp (getLocA ldecl)}
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
              GRHS _ [] body -> holdsLogic body
              GRHS {} -> True
          _ -> True
  _ -> True

-- | Whether this expression decides anything, as opposed to naming what to
-- run and what to run it on.
--
-- Anywhere in the expression, however deeply: a do-block inside an argument is
-- as unreachable to a test as one at the top.
holdsLogic :: LHsExpr GhcPs -> Bool
holdsLogic body = not (null (listify decides body))
  where
    decides :: HsExpr GhcPs -> Bool
    decides = \case
      HsDo {} -> True
      HsLam {} -> True
      HsCase {} -> True
      HsIf {} -> True
      HsMultiIf {} -> True
      HsLet {} -> True
      _ -> False
