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
spanOfSrcSpan rp s = case s of
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
    (d : _) -> ScopeOfDecl ref (declFactName d)
    [] -> ScopeOfFile ref
  where
    line :: Word
    line = positionLine (spanStart sp)

    spansLine :: DeclFact -> Bool
    spansLine d =
      positionLine (spanStart (declFactSpan d)) <= line
        && line <= positionLine (spanEnd (declFactSpan d))

rdrText :: Rdr.RdrName -> Text
rdrText = T.pack . Occ.occNameString . Rdr.rdrNameOcc

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
