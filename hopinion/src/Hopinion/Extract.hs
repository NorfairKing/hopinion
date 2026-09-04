{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Turning a parse tree into facts.
--
-- This module grows with every new fact and never with a new rule, which is
-- the trade that keeps a rule small. It is therefore also the part to review
-- hardest: a fact that is wrong here is wrong for every rule that reads it.
module Hopinion.Extract
  ( ExtractInput (..),
    extractModuleContext,
    failedModuleContext,
    preprocessedModuleContext,
    instanceDeclName,
  )
where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Hs
import qualified GHC.Types.Name.Occurrence as Occ
import qualified GHC.Types.Name.Reader as Rdr
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import qualified GHC.Types.SrcLoc as SrcLoc
import Hopinion.Annotation (annotationsOf)
import Hopinion.Comment
import Hopinion.Facts
import Hopinion.Parse
import Hopinion.Rule (RuleSet)
import Path (File, Path, Rel)

data ExtractInput = ExtractInput
  { extractInputModule :: !ModuleKey,
    extractInputRelPath :: !(Path Rel File),
    extractInputComponent :: !ComponentKind,
    extractInputComponentName :: !ComponentName,
    -- | The rules this run is made of, which is what decides whether a comment
    -- that looks like a suppression is one, names a rule that has been turned
    -- off, or names nothing at all.
    extractInputRules :: !RuleSet
  }

extractModuleContext :: ExtractInput -> ParsedModule -> ModuleContext
extractModuleContext input parsed =
  let rp = extractInputRelPath input
      mk = extractInputModule input
      ref = ModuleRef {moduleRefComponent = extractInputComponentName input, moduleRefModule = mk}
      decls = concatMap (declFactsOf rp) (hsmodDecls (unLoc (parsedModuleAst parsed)))
      ctx =
        mkCommentContext
          (T.lines (parsedModuleSource parsed))
          (parsedModuleComments parsed)
          decls
          (parsedModuleExportListSpan parsed)
          (parsedModuleHeaderLine parsed)
      attached = attachComments ctx (parsedModuleComments parsed)
      (annotations, annotationProblems) = annotationsOf (extractInputRules input) ref attached
   in ModuleContext
        { moduleContextModule = mk,
          moduleContextPath = rp,
          moduleContextComponent = extractInputComponent input,
          moduleContextComponentName = extractInputComponentName input,
          moduleContextInstances = concatMap (instanceFactsOf rp ref) (hsmodDecls (unLoc (parsedModuleAst parsed))),
          moduleContextNames = map (nameFactOf ref decls) (parsedModuleNames parsed),
          moduleContextComments = attached,
          moduleContextAnnotations = annotations,
          moduleContextAnnotationProblems = annotationProblems,
          moduleContextTypeApps = parsedModuleTypeApps parsed,
          moduleContextTemplateHaskell = parsedModuleTemplateHaskell parsed,
          moduleContextOutcome = ParsedOk
        }

-- | A module that did not parse still produces facts, because the project
-- layer has to treat the failure as an error rather than as a module with no
-- declarations.
failedModuleContext :: ExtractInput -> Position -> Text -> ModuleContext
failedModuleContext input errLoc errMsg = (emptyModuleContext input) {moduleContextOutcome = ParseFailed errLoc errMsg}

-- | A module that is declared and present but holds no Haskell to read.
preprocessedModuleContext :: ExtractInput -> ModuleContext
preprocessedModuleContext input = (emptyModuleContext input) {moduleContextOutcome = NotHaskellSource}

emptyModuleContext :: ExtractInput -> ModuleContext
emptyModuleContext input =
  ModuleContext
    { moduleContextModule = extractInputModule input,
      moduleContextPath = extractInputRelPath input,
      moduleContextComponent = extractInputComponent input,
      moduleContextComponentName = extractInputComponentName input,
      moduleContextInstances = [],
      moduleContextNames = [],
      moduleContextComments = [],
      moduleContextAnnotations = [],
      moduleContextAnnotationProblems = [],
      moduleContextTypeApps = [],
      moduleContextTemplateHaskell = NoTemplateHaskell,
      moduleContextOutcome = ParsedOk
    }

nameFactOf :: ModuleRef -> [DeclFact] -> NameOccurrence -> NameFact
nameFactOf ref decls occ =
  NameFact
    { nameFactText = nameOccurrenceText occ,
      nameFactSpan = nameOccurrenceSpan occ,
      nameFactScope = declScopeOf ref decls (nameOccurrenceSpan occ)
    }

-- | The declaration a span falls inside, which is where an annotation about
-- something written there has to go.
--
-- Whole lines rather than columns, because a suppression is written above a
-- line and a declaration is what a reader would put it above.
declScopeOf :: ModuleRef -> [DeclFact] -> Span -> ScopeKey
declScopeOf ref decls sp =
  case [d | d <- decls, spansLine d] of
    (d : _) -> ScopeOfDecl ref (declFactName d)
    [] -> ScopeOfFile ref
  where
    line :: Word
    line = positionLine (spanStart sp)

    spansLine :: DeclFact -> Bool
    spansLine d =
      positionLine (spanStart (declFactSpan d)) <= line
        && line <= positionLine (spanEnd (declFactSpan d))

spanOfLocated :: Path Rel File -> LHsDecl GhcPs -> Span
spanOfLocated rp ldecl = spanOfSrcSpan rp (getLocA ldecl)

spanOfSrcSpan :: Path Rel File -> SrcLoc.SrcSpan -> Span
spanOfSrcSpan rp s = case s of
  SrcLoc.RealSrcSpan rss _ ->
    Span
      { spanFile = rp,
        spanStart = positionFromGhc (SrcLoc.srcSpanStartLine rss) (SrcLoc.srcSpanStartCol rss),
        spanEnd = positionFromGhc (SrcLoc.srcSpanEndLine rss) (SrcLoc.srcSpanEndCol rss)
      }
  SrcLoc.UnhelpfulSpan _ -> wholeFileSpan rp

rdrText :: Rdr.RdrName -> Text
rdrText = T.pack . Occ.occNameString . Rdr.rdrNameOcc

-- | The name an instance declaration is known by, shared by the declaration
-- list and by every instance fact, so that a comment above an instance and a
-- finding at that instance agree on one scope key.
instanceDeclName :: Text -> TypeHead -> DeclName
instanceDeclName cls th = DeclName (T.pack (unwords ["instance", T.unpack cls, T.unpack (typeHeadText th)]))

declFactsOf :: Path Rel File -> LHsDecl GhcPs -> [DeclFact]
declFactsOf rp ldecl =
  let sp = spanOfLocated rp ldecl
      one name kind = [DeclFact {declFactName = name, declFactKind = kind, declFactSpan = sp}]
   in case unLoc ldecl of
        TyClD _ d -> case d of
          FamDecl _ fd -> one (DeclName (rdrText (unLoc (fdLName fd)))) DeclOther
          SynDecl {tcdLName = n} -> one (DeclName (rdrText (unLoc n))) DeclTypeSynonym
          DataDecl {tcdLName = n, tcdDataDefn = defn} ->
            one (DeclName (rdrText (unLoc n))) (dataOrNewtype defn)
          ClassDecl {tcdLName = n} -> one (DeclName (rdrText (unLoc n))) DeclClass
        InstD _ d -> case d of
          ClsInstD _ ci -> case sigTypeHead (cid_poly_ty ci) of
            Just (cls, th) -> one (instanceDeclName cls th) DeclInstance
            Nothing -> one (DeclName "instance") DeclInstance
          DataFamInstD _ _ -> one (DeclName "data family instance") DeclOther
          TyFamInstD _ _ -> one (DeclName "type family instance") DeclOther
        DerivD _ dd -> case sigTypeHead (dropWildCard (deriv_type dd)) of
          Just (cls, th) -> one (instanceDeclName cls th) DeclInstance
          Nothing -> one (DeclName "deriving instance") DeclInstance
        ValD _ b -> case b of
          FunBind {fun_id = n} -> one (DeclName (rdrText (unLoc n))) DeclValue
          PatBind {} -> one (DeclName "pattern binding") DeclValue
          PatSynBind _ psb -> one (DeclName (rdrText (unLoc (psb_id psb)))) DeclPattern
          VarBind {} -> one (DeclName "variable binding") DeclValue
        SigD _ s -> case s of
          TypeSig _ ns _ ->
            [ DeclFact {declFactName = DeclName (rdrText (unLoc n)), declFactKind = DeclSignature, declFactSpan = sp}
            | n <- ns
            ]
          PatSynSig _ ns _ ->
            [ DeclFact {declFactName = DeclName (rdrText (unLoc n)), declFactKind = DeclSignature, declFactSpan = sp}
            | n <- ns
            ]
          _ -> one (DeclName "signature") DeclOther
        ForD _ _ -> one (DeclName "foreign") DeclForeign
        _ -> one (DeclName "declaration") DeclOther

dataOrNewtype :: HsDataDefn GhcPs -> DeclKind
dataOrNewtype defn = case dd_cons defn of
  NewTypeCon _ -> DeclNewtype
  DataTypeCons _ _ -> DeclData

dropWildCard :: LHsSigWcType GhcPs -> LHsSigType GhcPs
dropWildCard = hswc_body

instanceFactsOf :: Path Rel File -> ModuleRef -> LHsDecl GhcPs -> [InstanceFact]
instanceFactsOf rp ref ldecl =
  let sp = spanOfLocated rp ldecl
   in case unLoc ldecl of
        InstD _ (ClsInstD _ ci) -> case sigTypeHead (cid_poly_ty ci) of
          Just (cls, th) ->
            [ InstanceFact
                { instanceFactClass = cls,
                  instanceFactType = th,
                  instanceFactOrigin = OriginInstanceDecl (instanceMethodsOf ci),
                  instanceFactSpan = sp,
                  instanceFactScope = ScopeOfDecl ref (instanceDeclName cls th)
                }
            ]
          Nothing -> []
        DerivD _ dd -> case sigTypeHead (dropWildCard (deriv_type dd)) of
          Just (cls, th) ->
            [ InstanceFact
                { instanceFactClass = cls,
                  instanceFactType = th,
                  instanceFactOrigin = standaloneOrigin (deriv_strategy dd),
                  instanceFactSpan = sp,
                  instanceFactScope = ScopeOfDecl ref (instanceDeclName cls th)
                }
            ]
          Nothing -> []
        TyClD _ DataDecl {tcdLName = n, tcdDataDefn = defn} ->
          let subject = TypeHead (rdrText (unLoc n))
              scope = ScopeOfDecl ref (DeclName (rdrText (unLoc n)))
           in [ InstanceFact
                  { instanceFactClass = cls,
                    -- The instance head type is the enclosing declaration. The
                    -- type in a via clause is the representation, and reading
                    -- it as the subject would produce obligations nothing can
                    -- satisfy.
                    instanceFactType = subject,
                    instanceFactOrigin = origin,
                    instanceFactSpan = sp,
                    instanceFactScope = scope
                  }
              | (cls, origin) <- derivedClasses defn
              ]
        _ -> []

-- | Whether every method in an instance body discards all of its arguments.
--
-- A wildcard rather than an unused name, because a name is only unused after
-- reading the body it is in scope over, and the two say the same thing to a
-- reader.
instanceMethodsOf :: ClsInstDecl GhcPs -> InstanceMethods
instanceMethodsOf ci =
  let binds = cid_binds ci
   in if not (null binds) && all (bindIgnoresArguments . unLoc) binds
        then MethodsIgnoreArguments
        else MethodsUseArguments

bindIgnoresArguments :: HsBind GhcPs -> Bool
bindIgnoresArguments = \case
  FunBind {fun_matches = mg} ->
    let alts = unLoc (mg_alts mg)
     in not (null alts) && all (matchIgnoresArguments . unLoc) alts
  PatBind {} -> False
  VarBind {} -> False
  PatSynBind {} -> False

-- | A match with no patterns at all binds the method to something rather than
-- taking the value apart, and what that something does with it is not here.
matchIgnoresArguments :: Match GhcPs (LHsExpr GhcPs) -> Bool
matchIgnoresArguments m =
  let pats = unLoc (m_pats m)
   in not (null pats) && all (isWildPat . unLoc) pats

isWildPat :: Pat GhcPs -> Bool
isWildPat = \case
  WildPat _ -> True
  _ -> False

standaloneOrigin :: Maybe (LDerivStrategy GhcPs) -> InstanceOrigin
standaloneOrigin = \case
  Just (L _ (ViaStrategy (XViaStrategyPs _ sigTy))) -> OriginDerivingVia (viaHead sigTy)
  _ -> OriginStandaloneDeriving

derivedClasses :: HsDataDefn GhcPs -> [(Text, InstanceOrigin)]
derivedClasses defn =
  [ (cls, origin)
  | L _ clause <- dd_derivs defn,
    let origin = clauseOrigin (deriv_clause_strategy clause),
    cls <- mapMaybe className (clauseTypes (deriv_clause_tys clause))
  ]
  where
    className sigTy = fmap fst (sigTypeHeadOrClassOnly sigTy)

clauseTypes :: LDerivClauseTys GhcPs -> [LHsSigType GhcPs]
clauseTypes (L _ dct) = case dct of
  DctSingle _ t -> [t]
  DctMulti _ ts -> ts

clauseOrigin :: Maybe (LDerivStrategy GhcPs) -> InstanceOrigin
clauseOrigin = \case
  Nothing -> OriginDerivingUnspecified
  Just (L _ s) -> case s of
    StockStrategy _ -> OriginDerivingStock
    AnyclassStrategy _ -> OriginDerivingAnyclass
    NewtypeStrategy _ -> OriginDerivingNewtype
    ViaStrategy (XViaStrategyPs _ sigTy) -> OriginDerivingVia (viaHead sigTy)

-- | A derived class name applied to nothing, as it appears in a deriving
-- clause, where the subject type is the enclosing declaration rather than an
-- argument.
sigTypeHeadOrClassOnly :: LHsSigType GhcPs -> Maybe (Text, [LHsType GhcPs])
sigTypeHeadOrClassOnly sigTy = case unLoc sigTy of
  HsSig {sig_body = body} -> peelApp (peelType body)

-- | The representation type of a @via@ clause. Recorded so the origin says
-- what the source said, never used as the instance's subject.
viaHead :: LHsSigType GhcPs -> TypeHead
viaHead sigTy = TypeHead (maybe "?" fst (sigTypeHeadOrClassOnly sigTy))

-- | The class and the head of its first argument, which is what an instance
-- declaration's obligation is keyed on.
sigTypeHead :: LHsSigType GhcPs -> Maybe (Text, TypeHead)
sigTypeHead sigTy = do
  (cls, args) <- sigTypeHeadOrClassOnly sigTy
  case args of
    (a : _) -> do
      (subject, _) <- peelApp (peelType a)
      pure (cls, TypeHead subject)
    [] -> Nothing

-- | Strip the parts of a type that do not change what it is about: parens,
-- foralls, contexts and kind signatures.
peelType :: LHsType GhcPs -> LHsType GhcPs
peelType lt = case unLoc lt of
  HsParTy _ t -> peelType t
  HsForAllTy {hst_body = t} -> peelType t
  HsQualTy {hst_body = t} -> peelType t
  HsKindSig _ t _ -> peelType t
  HsDocTy _ t _ -> peelType t
  HsBangTy _ _ t -> peelType t
  _ -> lt

-- | The head type constructor and its arguments, with applications flattened.
peelApp :: LHsType GhcPs -> Maybe (Text, [LHsType GhcPs])
peelApp lt = go lt []
  where
    go t acc = case unLoc (peelType t) of
      HsTyVar _ _ n -> Just (rdrText (unLoc n), acc)
      HsAppTy _ f x -> go f (x : acc)
      HsAppKindTy _ f _ -> go f acc
      HsOpTy _ _ _ n _ -> Just (rdrText (unLoc n), acc)
      HsListTy _ _ -> Just ("[]", acc)
      HsTupleTy {} -> Just ("(,)", acc)
      HsFunTy {} -> Just ("->", acc)
      _ -> Nothing
