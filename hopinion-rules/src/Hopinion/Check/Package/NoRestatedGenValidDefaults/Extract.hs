{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks what an instance method is defined as.
module Hopinion.Check.Package.NoRestatedGenValidDefaults.Extract (restatedMethodsOf) where

import Data.Text (Text)
import GHC.Hs
import GHC.Types.SrcLoc (unLoc)
import Hopinion.Check.Package.NoRestatedGenValidDefaults.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Instance
import Hopinion.Facts.Module
import Hopinion.Parse (ParsedModule, parsedModuleDecls)

-- | Every method of an instance of a class named GenValid that is defined as
-- the function that method uses by default.
--
-- The instance is found among the module's instance facts by its span, so that
-- what its class and type are has the one answer the shared extraction gives.
restatedMethodsOf :: ModuleContext -> ParsedModule -> [RestatedMethod]
restatedMethodsOf ctx parsed =
  let rp = moduleContextPath ctx
   in [ RestatedMethod
          { restatedMethodInstance = inst,
            restatedMethodName = defaultMethod d,
            restatedMethodSpan = spanOfSrcSpan rp (getLocA lbind)
          }
      | ldecl <- parsedModuleDecls parsed,
        InstD _ (ClsInstD _ ci) <- [unLoc ldecl],
        inst <- moduleContextInstances ctx,
        instanceFactSpan inst == spanOfLocated rp ldecl,
        instanceFactClass inst == "GenValid",
        lbind <- cid_binds ci,
        d <- defaults,
        restates d (unLoc lbind)
      ]

-- | One method's default definition, where the definition is one function.
data Default = Default
  { defaultMethod :: !Text,
    defaultFunction :: !Text
  }

-- | Read off genvalidity-1.1, where both methods default to the structural
-- functions by name.
--
-- Validity's @validate@ is not here: every way of restating its default is
-- also a @validate@ that does not extend @genericValidate@, which the rule
-- asking for that shape reports.
defaults :: [Default]
defaults =
  [ Default
      { defaultMethod = "genValid",
        defaultFunction = "genValidStructurally"
      },
    Default
      { defaultMethod = "shrinkValid",
        defaultFunction = "shrinkValidStructurally"
      }
  ]

-- | Whether this binding defines the default's method as the default's
-- function, either as the function itself or applied to the method's one
-- argument.
restates :: Default -> HsBind GhcPs -> Bool
restates d = \case
  FunBind {fun_id = name, fun_matches = mg} ->
    rdrText (unLoc name) == defaultMethod d
      && case unLoc (mg_alts mg) of
        [lm] -> equationRestates (defaultFunction d) (unLoc lm)
        _ -> False
  PatBind {} -> False
  VarBind {} -> False
  PatSynBind {} -> False

-- | An equation with no guards and no where clause, whose body is the function
-- when it takes no arguments, or the function applied to its argument when it
-- names exactly one.
equationRestates :: Text -> Match GhcPs (LHsExpr GhcPs) -> Bool
equationRestates function m = case (unLoc (m_pats m), m_grhss m) of
  (pats, GRHSs {grhssGRHSs = [lgrhs], grhssLocalBinds = EmptyLocalBinds _}) -> case unLoc lgrhs of
    GRHS _ [] body -> case pats of
      [] -> isFunction function body
      [lpat] -> case (unLoc lpat, unLoc (peelExpr body)) of
        (VarPat _ argument, HsApp _ f x) ->
          isFunction function f && isVariable (rdrText (unLoc argument)) x
        _ -> False
      _ -> False
    GRHS {} -> False
  _ -> False
