{-# LANGUAGE OverloadedStrings #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks which argument of a type is its error. Which types are prose, and which
-- declarations name an error, are shared with another rule and come from
-- 'Hopinion.Check.ProseError.Extract'.
module Hopinion.Check.Hs.NoStringError.Extract (errorSlotsOf) where

import Data.List (nubBy)
import Data.Maybe (isJust)
import Data.Text (Text)
import GHC.Hs
import GHC.Types.SrcLoc (unLoc)
import qualified GHC.Types.SrcLoc as SrcLoc
import Hopinion.Check.Hs.NoStringError.Fact
import Hopinion.Check.ProseError.Extract (errorTypeOf)
import Hopinion.Check.ProseError.Fact (ErrorType (..))
import Hopinion.Extract.Ghc
import Hopinion.Facts.Module

-- | Every error argument the module writes down, one fact per argument.
--
-- From the type syntax rather than from the names in it, because the question
-- is which argument of which type a name is written as: the @String@ in
-- @Either String a@ is an error and the one in @Either a String@ is a result.
errorSlotsOf :: ModuleContext -> [LHsType GhcPs] -> [ErrorSlot]
errorSlotsOf ctx types =
  let rp = moduleContextPath ctx
      ref = moduleContextRef ctx
      decls = moduleContextDecls ctx
   in [ ErrorSlot
          { errorSlotCarrier = carrier,
            errorSlotError = carrierErrorType carrier arg,
            errorSlotSpan = sp,
            errorSlotScope = declScopeOf ref decls sp
          }
      | t <- outermostApplications,
        Just (carrier, arg) <- [errorArgumentOf t],
        let sp = spanOfSrcSpan rp (getLocA arg)
      ]
  where
    -- Peeled before anything is asked of them, because a strict field is a
    -- bang around parentheses around the application, and all three answer for
    -- the same error argument. One entry per application, keyed on where it is.
    applications :: [LHsType GhcPs]
    applications =
      nubBy
        (\a b -> getLocA a == getLocA b)
        (map peelType (filter (isJust . errorArgumentOf) types))

    -- The function part of an application is a partial application of the same
    -- head, so reading it as well would report the one @String@ in
    -- @Either String a@ twice.
    nested :: [SrcLoc.SrcSpan]
    nested =
      [ getLocA (peelType f)
      | t <- applications,
        HsAppTy _ f _ <- [unLoc t]
      ]

    outermostApplications :: [LHsType GhcPs]
    outermostApplications = [t | t <- applications, getLocA t `notElem` nested]

-- | The type's name and the argument it carries its error in, for the types
-- that carry one at all.
--
-- A list rather than a question put to the compiler: these are the shapes a
-- reader recognises on sight, the error is the first argument of every one of
-- them, and a type of your own that carries an error somewhere else is not what
-- this rule is about.
--
-- Qualified or not: 'peelApp' answers with the occurrence name, so
-- @Control.Monad.Except.ExceptT@ arrives here as @ExceptT@.
errorArgumentOf :: LHsType GhcPs -> Maybe (Text, LHsType GhcPs)
errorArgumentOf t = case peelApp t of
  Just (carrier, arg : _)
    | carrier `elem` ["Either", "Except", "ExceptT", "MonadError", "Validation"] ->
        Just (carrier, arg)
  _ -> Nothing

-- | What a carrier's error argument says, which for one carrier depends on
-- which carrier it is.
--
-- @base@, @mtl@ and @transformers@ fix the argument order of the other four, so
-- a name matched on its own is the type we think it is. Nothing fixes
-- @Validation@: it is a name several packages define, and a type that put its
-- success first would read the same way here. What tells the orders apart is
-- that this one accumulates, so only a container of prose is read as prose. A
-- @Validation@ that does not accumulate is an 'Either' with more syllables, so
-- the recall that costs is recall over code nobody writes.
carrierErrorType :: Text -> LHsType GhcPs -> ErrorType
carrierErrorType carrier arg =
  if carrier == "Validation"
    then maybe ErrorTypeSomethingElse errorTypeOf (containerElement arg)
    else errorTypeOf arg

-- | The element of a type written as a list or a 'NonEmpty' of something.
--
-- A @String@ is not one of those: @[Char]@ is a single message spelled out
-- rather than a collection of them.
containerElement :: LHsType GhcPs -> Maybe (LHsType GhcPs)
containerElement lt = case unLoc (peelType lt) of
  HsListTy _ el -> if isCharType el then Nothing else Just el
  _ -> case peelApp lt of
    Just ("NonEmpty", [el]) -> Just el
    _ -> Nothing

isCharType :: LHsType GhcPs -> Bool
isCharType el = case peelApp el of
  Just ("Char", []) -> True
  _ -> False
