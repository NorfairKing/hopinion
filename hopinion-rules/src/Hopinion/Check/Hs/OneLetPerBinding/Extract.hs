{-# LANGUAGE LambdaCase #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks how many bindings a do-block's @let@ holds. What is shared is how a
-- span is placed, and that comes from 'Hopinion.Extract.Ghc'.
module Hopinion.Check.Hs.OneLetPerBinding.Extract (crowdedLetsOf) where

import GHC.Hs
import GHC.Types.SrcLoc (unLoc)
import Hopinion.Check.Hs.OneLetPerBinding.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Module

-- | Every @let@ statement of a do-block in the module that holds more than one
-- binding, wherever the do-block is written.
--
-- Over the expressions rather than over the statements, because a guard and a
-- comprehension are written with the same statements, and only a do-block is
-- what this rule is about.
crowdedLetsOf :: ModuleContext -> [LHsExpr GhcPs] -> [CrowdedLet]
crowdedLetsOf ctx exprs =
  let rp = moduleContextPath ctx
      ref = moduleContextRef ctx
      decls = moduleContextDecls ctx
   in [ CrowdedLet
          { crowdedLetBindings = bindings,
            crowdedLetSpan = sp,
            crowdedLetScope = declScopeOf ref decls sp
          }
      | le <- exprs,
        stmt <- doStatementsOf (unLoc le),
        let bindings = bindingsIn (unLoc stmt),
        bindings > 1,
        let sp = spanOfSrcSpan rp (getLocA stmt)
      ]

-- | The statements of a do-block, or none when this is not one.
--
-- An @mdo@ block is left alone: every binding in it already sees every other,
-- so splitting a @let@ there changes nothing about what can refer to what.
doStatementsOf :: HsExpr GhcPs -> [ExprLStmt GhcPs]
doStatementsOf = \case
  HsDo _ flavour stmts -> case flavour of
    DoExpr _ -> unLoc stmts
    MDoExpr _ -> []
    GhciStmtCtxt -> []
    ListComp -> []
    MonadComp -> []
  _ -> []

-- | How many bindings this statement introduces with @let@.
--
-- Bindings rather than variables, because a pattern binding such as
-- @let (a, b) = pair@ is one binding and cannot be split into two without a
-- name for the pair. Type signatures are not bindings and are not counted, and
-- implicit parameters are never recursive, so they are not counted either.
bindingsIn :: ExprStmt GhcPs -> Word
bindingsIn = \case
  LetStmt _ (HsValBinds _ (ValBinds _ binds _)) -> fromIntegral (length binds)
  _ -> 0
