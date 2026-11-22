{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks what an instance method is defined as.
module Hopinion.Check.Package.NoRestatedValidityDefaults.Extract (restatedMethodsOf) where

import Data.Text (Text)
import GHC.Hs
import GHC.Types.SrcLoc (unLoc)
import Hopinion.Check.Package.NoRestatedValidityDefaults.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Instance
import Hopinion.Facts.Module
import Hopinion.Parse (ParsedModule, parsedModuleDecls)

-- | Every method of an instance that is defined as the function a class of
-- that name uses by default.
--
-- The instance is found among the module's instance facts by its span, so that
-- what its class and type are has the one answer the shared extraction gives.
restatedMethodsOf :: ModuleContext -> ParsedModule -> [RestatedMethod]
restatedMethodsOf ctx parsed =
  let rp = moduleContextPath ctx
   in [ RestatedMethod
          { restatedMethodInstance = inst,
            restatedMethodClassModule = defaultClassModule d,
            restatedMethodName = defaultMethod d,
            restatedMethodSpan = spanOfSrcSpan rp (getLocA lbind)
          }
      | ldecl <- parsedModuleDecls parsed,
        InstD _ (ClsInstD _ ci) <- [unLoc ldecl],
        inst <- moduleContextInstances ctx,
        instanceFactSpan inst == spanOfLocated rp ldecl,
        lbind <- cid_binds ci,
        d <- defaults,
        defaultClass d == instanceFactClass inst,
        restates d (unLoc lbind)
      ]

-- | One method's default definition, where the definition is one function.
data Default = Default
  { defaultClassModule :: !Text,
    defaultClass :: !Text,
    defaultMethod :: !Text,
    defaultFunction :: !Text
  }

-- | Read off validity-0.12 and genvalidity-1.1.
-- @validate@ defaults to the composition @genericValidate@ is defined as, and
-- the two GenValid methods default to the structural functions by name.
defaults :: [Default]
defaults =
  [ Default
      { defaultClassModule = "Data.Validity",
        defaultClass = "Validity",
        defaultMethod = "validate",
        defaultFunction = "genericValidate"
      },
    Default
      { defaultClassModule = "Data.GenValidity",
        defaultClass = "GenValid",
        defaultMethod = "genValid",
        defaultFunction = "genValidStructurally"
      },
    Default
      { defaultClassModule = "Data.GenValidity",
        defaultClass = "GenValid",
        defaultMethod = "shrinkValid",
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

-- | Whether this expression is the function, with any parentheses, type
-- signature or type application around it: none of those change which
-- function it is.
isFunction :: Text -> LHsExpr GhcPs -> Bool
isFunction function le = case unLoc (peelExpr le) of
  HsAppType _ f _ -> isFunction function f
  _ -> isVariable function le

isVariable :: Text -> LHsExpr GhcPs -> Bool
isVariable name le = case unLoc (peelExpr le) of
  HsVar _ n -> rdrText (unLoc n) == name
  _ -> False
