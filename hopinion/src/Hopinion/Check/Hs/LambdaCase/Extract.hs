{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks what a function does with its last argument. What is shared is how an
-- expression is taken apart, and that comes from 'Hopinion.Extract.Ghc'.
module Hopinion.Check.Hs.LambdaCase.Extract (casedArgumentsOf) where

import Data.Data (Data)
import Data.Generics (listify)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Data.Text (Text)
import GHC.Hs
import GHC.Types.Fixity (LexicalFixity (..))
import GHC.Types.Name.Reader (RdrName)
import GHC.Types.SrcLoc (unLoc)
import Hopinion.Check.Hs.LambdaCase.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Decl
import Hopinion.Facts.Place
import Path (File, Path, Rel)

-- | Every binding in the module that names its last argument only to take it
-- apart, wherever it is written: a where clause and an instance method name
-- arguments the same way a top-level function does.
--
-- Over the bindings rather than over the declarations, because a top-level
-- binding and a local one are the same node once the declaration wrapper is
-- off, so one predicate answers for both.
casedArgumentsOf :: Path Rel File -> ModuleRef -> [DeclFact] -> [LHsDecl GhcPs] -> [CasedArgument]
casedArgumentsOf rp ref decls ds =
  [ CasedArgument
      { casedArgumentShape = shape,
        casedArgumentSpan = sp,
        casedArgumentScope = declScopeOf ref decls sp
      }
  | b <- listify (isJust . shapeOf) ds,
    Just (shape, name) <- [shapeOf b],
    let sp = spanOfSrcSpan rp (getLocA name)
  ]

-- | Which shape this binding is, and the name it repeats.
--
-- The name rather than the equations, because the equations of a where-bound
-- helper reach to the end of its own where clause and so contain the bindings
-- in it, and a finding drawn over another finding reads as one thing to fix
-- rather than as three.
--
-- Infix definitions are left alone: @\\case@ has no infix form, so there is
-- nothing to prefer it over.
shapeOf :: HsBind GhcPs -> Maybe (ArgumentShape, LIdP GhcPs)
shapeOf = \case
  FunBind {fun_id = name, fun_matches = mg} -> do
    ms <- NE.nonEmpty (unLoc (mg_alts mg))
    if not (all (isPrefixMatch . unLoc) ms)
      then Nothing
      else case ms of
        (m :| []) ->
          if namesThenCases (unLoc m) then Just (ArgumentNamedThenCased, name) else Nothing
        _ ->
          if splitOverEquations (fmap unLoc ms) then Just (ArgumentSplitOverEquations, name) else Nothing
  PatBind {} -> Nothing
  VarBind {} -> Nothing
  PatSynBind {} -> Nothing

isPrefixMatch :: Match GhcPs (LHsExpr GhcPs) -> Bool
isPrefixMatch m = case m_ctxt m of
  FunRhs {mc_fixity = Prefix} -> True
  _ -> False

-- | Whether these equations differ in their last argument alone.
--
-- Every argument but the last has to be named, and named the same thing in each
-- equation, because those are the arguments the lambda keeps. An equation that
-- takes an earlier one apart is doing something a single @\\case@ cannot say.
splitOverEquations :: NonEmpty (Match GhcPs (LHsExpr GhcPs)) -> Bool
splitOverEquations ms = case traverse leadingNames ms of
  Nothing -> False
  Just (leading :| rest) -> all (== leading) rest

-- | The names of every argument but the last, or nothing when the function
-- takes no arguments or takes an earlier one apart.
leadingNames :: Match GhcPs (LHsExpr GhcPs) -> Maybe [Text]
leadingNames m = case reverse (unLoc (m_pats m)) of
  [] -> Nothing
  (_ : leading) -> traverse (patName . unLoc) (reverse leading)

patName :: Pat GhcPs -> Maybe Text
patName = \case
  VarPat _ n -> Just (rdrText (unLoc n))
  _ -> Nothing

-- | Whether this one equation names its last argument and then does nothing
-- with it but @case@ on it.
--
-- The name having no other occurrence is what makes it only a name: a guard, a
-- where clause or an alternative that reads it is a use the lambda would have
-- nowhere to put. Any occurrence at all counts, shadowing included, so this
-- errs towards saying nothing.
namesThenCases :: Match GhcPs (LHsExpr GhcPs) -> Bool
namesThenCases m = case (reverse (unLoc (m_pats m)), m_grhss m) of
  (final : _, GRHSs {grhssGRHSs = [lgrhs], grhssLocalBinds = binds}) ->
    case (patName (unLoc final), unLoc lgrhs) of
      (Just name, GRHS _ [] body) -> case unLoc (peelExpr body) of
        HsCase _ scrutinee alternatives ->
          scrutinises name scrutinee
            && not (mentions name alternatives)
            && not (mentions name binds)
        _ -> False
      _ -> False
  _ -> False

scrutinises :: Text -> LHsExpr GhcPs -> Bool
scrutinises name le = case unLoc (peelExpr le) of
  HsVar _ n -> rdrText (unLoc n) == name
  _ -> False

-- | Whether this name is written anywhere in there.
--
-- Every occurrence of the spelling rather than only the ones that resolve to
-- the argument, because deciding which is which is renaming, and a rule that
-- has to rename to answer is one that can be wrong about code nobody would
-- want it to be wrong about.
mentions :: (Data a) => Text -> a -> Bool
mentions name x = any ((== name) . rdrText) (listify (const True :: RdrName -> Bool) x)
