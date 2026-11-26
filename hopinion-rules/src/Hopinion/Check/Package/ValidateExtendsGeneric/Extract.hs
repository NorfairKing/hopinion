{-# LANGUAGE OverloadedStrings #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks what shape a @validate@ has.
module Hopinion.Check.Package.ValidateExtendsGeneric.Extract (unshapedValidatesOf) where

import Data.Text (Text)
import GHC.Hs
import GHC.Types.SrcLoc (unLoc)
import Hopinion.Check.Package.ValidateExtendsGeneric.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Instance
import Hopinion.Facts.Module
import Hopinion.Parse (ParsedModule, parsedModuleDecls)

-- | Every @validate@ of an instance of a class named Validity that does not
-- extend the structural check.
--
-- The instance is found among the module's instance facts by its span, so that
-- what its class and type are has the one answer the shared extraction gives.
unshapedValidatesOf :: ModuleContext -> ParsedModule -> [UnshapedValidate]
unshapedValidatesOf ctx parsed =
  let rp = moduleContextPath ctx
   in [ UnshapedValidate
          { unshapedValidateInstance = inst,
            unshapedValidateShape = shape,
            unshapedValidateSpan = spanOfSrcSpan rp (getLocA lbind)
          }
      | ldecl <- parsedModuleDecls parsed,
        InstD _ (ClsInstD _ ci) <- [unLoc ldecl],
        inst <- moduleContextInstances ctx,
        instanceFactSpan inst == spanOfLocated rp ldecl,
        instanceFactClass inst == "Validity",
        lbind <- cid_binds ci,
        FunBind {fun_id = name, fun_matches = mg} <- [unLoc lbind],
        rdrText (unLoc name) == "validate",
        Just shape <- [unshapedValidateShapeOf mg]
      ]

-- | What a @validate@ is instead, or nothing when it is one equation that
-- names its one argument and is @mconcat@ of a literal list whose first
-- element is @genericValidate@ of that argument, followed by at least one
-- more.
--
-- At least one more, because a list of nothing but the structural check is the
-- default, written out.
unshapedValidateShapeOf :: MatchGroup GhcPs (LHsExpr GhcPs) -> Maybe ValidateShape
unshapedValidateShapeOf mg = case plainEquation mg of
  Just ([], body)
    | isFunction "genericValidate" body -> Just ValidateShapeRestatesDefault
    | otherwise -> Just ValidateShapeOther
  Just ([lpat], body) -> case argumentName lpat of
    Just argument
      | isGenericValidateOf argument body -> Just ValidateShapeRestatesDefault
      | otherwise -> case mconcatListOf body of
          Just [only]
            | isGenericValidateOf argument only -> Just ValidateShapeRestatesDefault
          Just (first : _ : _)
            | isGenericValidateOf argument first -> Nothing
          _ -> Just ValidateShapeOther
    Nothing -> Just ValidateShapeOther
  Just _ -> Just ValidateShapeOther
  Nothing -> Just ValidateShapeOther

-- | The patterns and the body of the one equation of a binding, when it has no
-- guards and no local bindings.
plainEquation :: MatchGroup GhcPs (LHsExpr GhcPs) -> Maybe ([LPat GhcPs], LHsExpr GhcPs)
plainEquation mg = case unLoc (mg_alts mg) of
  [lm] -> case m_grhss (unLoc lm) of
    GRHSs {grhssGRHSs = [lgrhs], grhssLocalBinds = EmptyLocalBinds _} -> case unLoc lgrhs of
      GRHS _ [] body -> Just (unLoc (m_pats (unLoc lm)), body)
      GRHS {} -> Nothing
    GRHSs {} -> Nothing
  _ -> Nothing

-- | The variable a pattern binds the whole argument to, under anything that
-- does not change what it binds.
argumentName :: LPat GhcPs -> Maybe Text
argumentName lp = case unLoc lp of
  VarPat _ n -> Just (rdrText (unLoc n))
  AsPat _ n _ -> Just (rdrText (unLoc n))
  ParPat _ p -> argumentName p
  BangPat _ p -> argumentName p
  LazyPat _ p -> argumentName p
  SigPat _ p _ -> argumentName p
  _ -> Nothing

-- | The elements of a literal list that @mconcat@ is applied to.
mconcatListOf :: LHsExpr GhcPs -> Maybe [LHsExpr GhcPs]
mconcatListOf le = case unLoc (peelExpr le) of
  HsApp _ f xs
    | isFunction "mconcat" f -> case unLoc (peelExpr xs) of
        ExplicitList _ elements -> Just elements
        _ -> Nothing
  _ -> Nothing

isGenericValidateOf :: Text -> LHsExpr GhcPs -> Bool
isGenericValidateOf argument le = case unLoc (peelExpr le) of
  HsApp _ f x -> isFunction "genericValidate" f && isVariable argument x
  _ -> False
