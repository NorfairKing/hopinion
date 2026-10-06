{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reading the parse tree, for whoever is reading it.
--
-- What is here is the part of that reading no rule owns. A rule's own
-- extraction lives beside that rule and asks this for the parts every rule
-- shares.
module Hopinion.Extract.Ghc
  ( spanOfSrcSpan,
    spanOfLocated,
    declScopeOf,
    rdrText,
    peelExpr,
    peelType,
    peelApp,
    infixOperands,
    spineOf,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Hs
import qualified GHC.Types.Name.Occurrence as Occ
import qualified GHC.Types.Name.Reader as Rdr
import GHC.Types.SrcLoc (unLoc)
import qualified GHC.Types.SrcLoc as SrcLoc
import Hopinion.Facts.Decl
import Hopinion.Facts.Place
import Path (File, Path, Rel)

spanOfSrcSpan :: Path Rel File -> SrcLoc.SrcSpan -> Span
spanOfSrcSpan rp = \case
  SrcLoc.RealSrcSpan rss _ ->
    Span
      { spanFile = rp,
        spanStart = positionFromGhc (SrcLoc.srcSpanStartLine rss) (SrcLoc.srcSpanStartCol rss),
        spanEnd = positionFromGhc (SrcLoc.srcSpanEndLine rss) (SrcLoc.srcSpanEndCol rss)
      }
  SrcLoc.UnhelpfulSpan _ -> wholeFileSpan rp

spanOfLocated :: Path Rel File -> LHsDecl GhcPs -> Span
spanOfLocated rp ldecl = spanOfSrcSpan rp (getLocA ldecl)

-- | The declaration a span falls inside, which is where an annotation about
-- something written there has to go.
--
-- Whole lines rather than columns, because a suppression is written above a
-- line and a declaration is what a reader would put it above.
declScopeOf :: ModuleRef -> [DeclFact] -> Span -> ScopeKey
declScopeOf ref decls sp =
  case [d | d <- decls, spansLine d] of
    (d : _) -> ScopeKeyOfDecl ref (declFactName d)
    [] -> ScopeKeyOfFile ref
  where
    line :: Word
    line = positionLine (spanStart sp)

    spansLine :: DeclFact -> Bool
    spansLine d =
      positionLine (spanStart (declFactSpan d)) <= line
        && line <= positionLine (spanEnd (declFactSpan d))

rdrText :: Rdr.RdrName -> Text
rdrText = T.pack . Occ.occNameString . Rdr.rdrNameOcc

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
--
-- A list, a tuple and a function type have a head that names the syntax rather
-- than a constructor that could be applied to anything, so what they are
-- written over is not among the arguments this returns.
peelApp :: LHsType GhcPs -> Maybe (Text, [LHsType GhcPs])
peelApp lt = go lt []
  where
    go :: LHsType GhcPs -> [LHsType GhcPs] -> Maybe (Text, [LHsType GhcPs])
    go t acc = case unLoc (peelType t) of
      HsTyVar _ _ n -> Just (rdrText (unLoc n), acc)
      HsAppTy _ f x -> go f (x : acc)
      HsAppKindTy _ f _ -> go f acc
      HsOpTy _ _ _ n _ -> Just (rdrText (unLoc n), acc)
      HsListTy _ _ -> Just ("[]", acc)
      HsTupleTy {} -> Just ("(,)", acc)
      HsFunTy {} -> Just ("->", acc)
      _ -> Nothing

-- | Neither parentheses nor a type signature change what an expression is.
peelExpr :: LHsExpr GhcPs -> LHsExpr GhcPs
peelExpr le = case unLoc le of
  HsPar _ e -> peelExpr e
  ExprWithTySig _ e _ -> peelExpr e
  _ -> le

-- | The operands of one infix application, whatever its operator.
infixOperands :: LHsExpr GhcPs -> Maybe (LHsExpr GhcPs, LHsExpr GhcPs)
infixOperands le = case unLoc le of
  OpApp _ l _ r -> Just (l, r)
  _ -> Nothing

-- | An infix spine in source order: its leftmost operand, then each operator
-- with the operand to its right.
--
-- Flattened with no regard for fixity, because the parser has resolved none of
-- it. GhcPs nests every infix application to the left whatever the real
-- associativity is, so @f $ "a" <> b@ arrives as @(f $ "a") <> b@ and the tree
-- says the literal is an operand of the @$@. Source order is what survives
-- fixity resolution, so source order is what this reads.
spineOf :: LHsExpr GhcPs -> (LHsExpr GhcPs, [(LHsExpr GhcPs, LHsExpr GhcPs)])
spineOf le = case unLoc (peelExpr le) of
  OpApp _ l op r ->
    let (leftmost, leftRest) = spineOf l
        (rightFirst, rightRest) = spineOf r
     in (leftmost, leftRest ++ ((op, rightFirst) : rightRest))
  _ -> (peelExpr le, [])
