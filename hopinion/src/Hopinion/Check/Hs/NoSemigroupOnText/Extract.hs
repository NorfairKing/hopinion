{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks what a concatenation is. What is shared is how an expression is taken
-- apart, and that comes from 'Hopinion.Extract.Ghc'.
module Hopinion.Check.Hs.NoSemigroupOnText.Extract (concatChainsOf) where

import Data.Generics (listify)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import GHC.Hs
import GHC.Types.SrcLoc (unLoc)
import qualified GHC.Types.SrcLoc as SrcLoc
import Hopinion.Check.Hs.NoSemigroupOnText.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Decl
import Hopinion.Facts.Place
import Path (File, Path, Rel)

-- | Every run of concatenation the module writes, one fact per run.
--
-- From the parse tree rather than from the tokens, because a token before an
-- operator is not an operand of it: @text "a" <> b@ has a string literal beside
-- the @<>@ and concatenates whatever @text@ returns.
--
-- Only the outermost infix expression of a spine is flattened, since an inner
-- one is part of that spine rather than a spine of its own.
concatChainsOf :: Path Rel File -> ModuleRef -> [DeclFact] -> [LHsDecl GhcPs] -> [ConcatChain]
concatChainsOf rp ref decls ds =
  [ ConcatChain
      { concatChainOperands = map operandOf (NE.toList run),
        concatChainSpan = sp,
        concatChainScope = declScopeOf ref decls sp
      }
  | spine <- outermostInfixes,
    run <- concatRuns spine,
    let sp = spanOfRun rp run
  ]
  where
    infixes :: [LHsExpr GhcPs]
    infixes = listify (isJust . infixOperands) ds

    nested :: [SrcLoc.SrcSpan]
    nested =
      [ getLocA operand
      | e <- infixes,
        Just (l, r) <- [infixOperands e],
        operand <- map peelExpr [l, r],
        isJust (infixOperands operand)
      ]

    outermostInfixes :: [LHsExpr GhcPs]
    outermostInfixes = [e | e <- infixes, getLocA e `notElem` nested]

-- | From the start of a run's first operand to the end of its last, which is
-- the concatenation rather than whatever else the spine it sits in holds.
spanOfRun :: Path Rel File -> NonEmpty (LHsExpr GhcPs) -> Span
spanOfRun rp run =
  let spans = fmap (spanOfSrcSpan rp . getLocA) run
   in (NE.head spans) {spanEnd = spanEnd (NE.last spans)}

-- | Whether an operator concatenates.
--
-- Qualified or not: the occurrence name of @Data.Semigroup.<>@ is the operator
-- itself, so both spellings answer here.
isConcatOperator :: LHsExpr GhcPs -> Bool
isConcatOperator le = case unLoc le of
  HsVar _ n -> rdrText (unLoc n) `elem` ["<>", "++"]
  _ -> False

-- | The operands of each maximal run of concatenation in a spine.
--
-- A run rather than the whole spine, because one spine can hold two
-- concatenations that have nothing to do with each other, as
-- @f $ "a" <> b == "c" <> d@ does.
concatRuns :: LHsExpr GhcPs -> [NonEmpty (LHsExpr GhcPs)]
concatRuns le =
  let (leftmost, joins) = spineOf le
   in outside leftmost joins
  where
    -- Two functions rather than a flag: whether a run is open is which of them
    -- is running.
    outside :: LHsExpr GhcPs -> [(LHsExpr GhcPs, LHsExpr GhcPs)] -> [NonEmpty (LHsExpr GhcPs)]
    outside _ [] = []
    outside previous ((op, next) : more)
      | isConcatOperator op = inside (next :| [previous]) more
      | otherwise = outside next more

    -- The run so far, backwards, so that a further operand is a cons.
    inside :: NonEmpty (LHsExpr GhcPs) -> [(LHsExpr GhcPs, LHsExpr GhcPs)] -> [NonEmpty (LHsExpr GhcPs)]
    inside sofar [] = [NE.reverse sofar]
    inside sofar ((op, next) : more)
      | isConcatOperator op = inside (NE.cons next sofar) more
      | otherwise = NE.reverse sofar : outside next more

operandOf :: LHsExpr GhcPs -> ConcatOperand
operandOf le = case unLoc (peelExpr le) of
  HsLit _ HsString {} -> OperandStringLiteral
  HsLit _ HsMultilineString {} -> OperandStringLiteral
  _ -> OperandSomethingElse
