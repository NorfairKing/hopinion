{-# LANGUAGE OverloadedStrings #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks what shape a @validate@ has.
module Hopinion.Check.Package.ValidateExtendsGeneric.Extract (unshapedValidatesOf) where

import Data.Text (Text)
import GHC.Hs
import qualified GHC.Types.Name.Reader as Rdr
import GHC.Types.SrcLoc (unLoc)
import Hopinion.Check.Package.ValidateExtendsGeneric.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Instance
import Hopinion.Facts.Module
import Hopinion.Facts.Place (Span)
import Hopinion.Parse (ParsedModule, parsedModuleDecls)

-- | Every @validate@ of an instance of a class named Validity that does not
-- extend the structural check, itself or through the top-level binding it
-- names.
--
-- The instance is found among the module's instance facts by its span, so that
-- what its class and type are has the one answer the shared extraction gives.
unshapedValidatesOf :: ModuleContext -> ParsedModule -> [UnshapedValidate]
unshapedValidatesOf ctx parsed =
  let rp = moduleContextPath ctx
      decls = parsedModuleDecls parsed
      writtenAt :: [LHsDecl GhcPs] -> LHsBind GhcPs -> MatchGroup GhcPs (LHsExpr GhcPs) -> (ValidateSite, Span, Maybe ValidateShape)
      writtenAt neighbours lbind mg =
        let method :: Maybe ValidateShape -> (ValidateSite, Span, Maybe ValidateShape)
            method shape = (ValidateSiteMethod, spanOfSrcSpan rp (getLocA lbind), shape)
         in case delegateOf mg of
              Just binding
                | ((ldecl, bindingMg) : _) <- bindingsNamed binding neighbours ->
                    (ValidateSiteBinding binding, spanOfLocated rp ldecl, unshapedValidateShapeOf bindingMg)
                | (_ : _) <- bindingsNamed binding decls -> method (Just ValidateShapeBindingApart)
              _ -> method (unshapedValidateShapeOf mg)
   in [ UnshapedValidate
          { unshapedValidateInstance = inst,
            unshapedValidateSite = site,
            unshapedValidateShape = shape,
            unshapedValidateSpan = sp
          }
      | (index, ldecl) <- zip [0 :: Int ..] decls,
        InstD _ (ClsInstD _ ci) <- [unLoc ldecl],
        inst <- moduleContextInstances ctx,
        instanceFactSpan inst == spanOfLocated rp ldecl,
        instanceFactClass inst == "Validity",
        lbind <- cid_binds ci,
        FunBind {fun_id = name, fun_matches = mg} <- [unLoc lbind],
        rdrText (unLoc name) == "validate",
        let neighbours = neighbouringBindings (reverse (take index decls)) ++ neighbouringBindings (drop (index + 1) decls),
        (site, sp, Just shape) <- [writtenAt neighbours lbind mg]
      ]

-- | The binding a declaration is next to on one side, given the declarations
-- on that side nearest first.
--
-- Next to it with nothing but signatures, pragmas and documentation between,
-- because those belong to the binding: an annotation on which mutations to
-- leave out is what a method names a binding for.
neighbouringBindings :: [LHsDecl GhcPs] -> [LHsDecl GhcPs]
neighbouringBindings sideways = case dropWhile isAside sideways of
  (ldecl : _)
    | ValD {} <- unLoc ldecl -> [ldecl]
  _ -> []

isAside :: LHsDecl GhcPs -> Bool
isAside ldecl = case unLoc ldecl of
  SigD {} -> True
  AnnD {} -> True
  DocD {} -> True
  _ -> False

-- | The name a @validate@ is nothing but, applied to its one argument or not.
--
-- Unqualified only, because a qualified name is not a binding of this module,
-- even when this module has a binding of the same name.
delegateOf :: MatchGroup GhcPs (LHsExpr GhcPs) -> Maybe Text
delegateOf mg = case plainEquation mg of
  Just ([], body) -> unqualifiedVariable body
  Just ([lpat], body) -> case (argumentName lpat, unLoc (peelExpr body)) of
    (Just argument, HsApp _ f x)
      | isVariable argument x -> unqualifiedVariable f
    _ -> Nothing
  Just _ -> Nothing
  Nothing -> Nothing

unqualifiedVariable :: LHsExpr GhcPs -> Maybe Text
unqualifiedVariable le = case unLoc (peelExpr le) of
  HsVar _ n
    | Rdr.isUnqual (unLoc n) -> Just (rdrText (unLoc n))
  _ -> Nothing

bindingsNamed :: Text -> [LHsDecl GhcPs] -> [(LHsDecl GhcPs, MatchGroup GhcPs (LHsExpr GhcPs))]
bindingsNamed wanted decls =
  [ (ldecl, mg)
  | ldecl <- decls,
    ValD _ FunBind {fun_id = name, fun_matches = mg} <- [unLoc ldecl],
    rdrText (unLoc name) == wanted
  ]

-- | What a @validate@ is instead, or nothing when it is one equation that
-- names its one argument and is @mconcat@ of a literal list whose first
-- element is @genericValidate@ of that argument, followed by at least one
-- more.
--
-- At least one more, because a list of nothing but the structural check is the
-- default, written out.
--
-- Nothing as well when it is @trivialValidation@, which says in its name that
-- it checks nothing, for a type that cannot start from the structural check.
unshapedValidateShapeOf :: MatchGroup GhcPs (LHsExpr GhcPs) -> Maybe ValidateShape
unshapedValidateShapeOf mg = case plainEquation mg of
  Just ([], body)
    | isFunction "trivialValidation" body -> Nothing
    | isFunction "genericValidate" body -> Just ValidateShapeRestatesDefault
    | isConstValid body -> Just ValidateShapeAlwaysValid
    | otherwise -> Just ValidateShapeOther
  Just ([_], body)
    | isFunction "valid" body -> Just ValidateShapeAlwaysValid
  Just ([lpat], body) -> case argumentName lpat of
    Just argument
      | isTrivialValidationOf argument body -> Nothing
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
isGenericValidateOf = isApplicationOf "genericValidate"

isTrivialValidationOf :: Text -> LHsExpr GhcPs -> Bool
isTrivialValidationOf = isApplicationOf "trivialValidation"

isApplicationOf :: Text -> Text -> LHsExpr GhcPs -> Bool
isApplicationOf function argument le = case unLoc (peelExpr le) of
  HsApp _ f x -> isFunction function f && isVariable argument x
  _ -> False

isConstValid :: LHsExpr GhcPs -> Bool
isConstValid le = case unLoc (peelExpr le) of
  HsApp _ f x -> isFunction "const" f && isFunction "valid" x
  _ -> False
