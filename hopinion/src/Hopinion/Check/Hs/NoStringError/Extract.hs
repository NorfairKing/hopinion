{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks which argument of a type is its error. What is shared is how a type is
-- taken apart, and that comes from 'Hopinion.Extract.Ghc'.
module Hopinion.Check.Hs.NoStringError.Extract (errorSlotsOf, errorDeclsOf) where

import Data.Generics (listify)
import Data.List (nubBy)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Hs
import GHC.Types.SrcLoc (unLoc)
import qualified GHC.Types.SrcLoc as SrcLoc
import Hopinion.Check.Hs.NoStringError.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Decl
import Hopinion.Facts.Place
import Path (File, Path, Rel)

-- | Every error argument the module writes down, one fact per argument.
--
-- From the type syntax rather than from the names in it, because the question
-- is which argument of which type a name is written as: the @String@ in
-- @Either String a@ is an error and the one in @Either a String@ is a result.
errorSlotsOf :: Path Rel File -> ModuleRef -> [DeclFact] -> [LHsDecl GhcPs] -> [ErrorSlot]
errorSlotsOf rp ref decls ds =
  [ ErrorSlot
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
        (map peelType (listify (isJust . errorArgumentOf) ds))

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

-- | What a slot's error is, seen through the containers that hold several of
-- them.
--
-- A list or a 'NonEmpty' of errors is as much prose as one error is, and the
-- accumulating shape is where @[String]@ gets written in the first place, so
-- the element is what gets read.
errorTypeOf :: LHsType GhcPs -> ErrorType
errorTypeOf lt = case unLoc (peelType lt) of
  HsListTy _ el -> elementErrorType el
  _ -> case peelApp lt of
    Just ("NonEmpty", [el]) -> elementErrorType el
    Just ("String", []) -> ErrorTypeString
    Just ("Text", []) -> ErrorTypeText
    _ -> ErrorTypeSomethingElse

-- | @[Char]@ is @String@ spelled out, and a list is the one place a @Char@ is
-- prose. On its own it is a character, which no error message is.
elementErrorType :: LHsType GhcPs -> ErrorType
elementErrorType el = case peelApp el of
  Just ("Char", []) -> ErrorTypeString
  _ -> errorTypeOf el

-- | Every declaration that calls itself an error and is prose underneath.
--
-- Read off the name, which is the one thing that says a type is meant as a
-- failure. That misses an error type named well, and catches the one written to
-- get out of the way of this rule: a signature reported for its @String@ is
-- answered by naming the @String@ and nothing else.
errorDeclsOf :: Path Rel File -> ModuleRef -> [DeclFact] -> [LHsDecl GhcPs] -> [ErrorDecl]
errorDeclsOf rp ref decls ds =
  [ ErrorDecl
      { errorDeclName = name,
        errorDeclShape = shape,
        errorDeclError = errorTypeOf prose,
        errorDeclSpan = sp,
        errorDeclScope = declScopeOf ref decls sp
      }
  | TyClD _ d <- map unLoc ds,
    Just (name, shape, prose) <- [declaredError d],
    isErrorName name,
    let sp = spanOfSrcSpan rp (getLocA prose)
  ]

-- | The name a declaration gives itself, how it is written, and the type it is
-- prose in if it is prose at all.
--
-- A data declaration answers only when its one constructor holds one thing and
-- is the type's own name again, which is the wrapper a reader learns nothing
-- from. A @data@ with two constructors is the fix this rule asks for.
declaredError :: TyClDecl GhcPs -> Maybe (Text, ErrorDeclShape, LHsType GhcPs)
declaredError = \case
  SynDecl {tcdLName = n, tcdRhs = rhs} ->
    Just (rdrText (unLoc n), ErrorDeclShapeSynonym, rhs)
  DataDecl {tcdLName = n, tcdDataDefn = defn} -> do
    let name = rdrText (unLoc n)
    con <- soleConstructor defn
    conName <- constructorName con
    if echoesTypeName name conName
      then do
        field <- soleField con
        pure (name, ErrorDeclShapeWrapper, field)
      else Nothing
  _ -> Nothing

-- | Whether a constructor's name says anything its type's name does not.
--
-- It does not when it is that name over again, with or without the @Mk@ a
-- wrapper is sometimes spelled with. Such a constructor holds the whole of what
-- the value is, so the prose under it is the message: a renderer for one of
-- these is @id@.
--
-- A constructor that names the failure is a different thing even when it is the
-- only one. @UnknownRepoType !Text@ holds the input that was not understood
-- rather than words about it, and what a reader is told comes from the renderer
-- that pattern-matches on the name. Reporting those would be reporting the fix.
echoesTypeName :: Text -> Text -> Bool
echoesTypeName typeName conName =
  conName == typeName || conName == T.pack (concat ["Mk", T.unpack typeName])

constructorName :: ConDecl GhcPs -> Maybe Text
constructorName = \case
  ConDeclH98 {con_name = n} -> Just (rdrText (unLoc n))
  ConDeclGADT {} -> Nothing

soleConstructor :: HsDataDefn GhcPs -> Maybe (ConDecl GhcPs)
soleConstructor defn = case dd_cons defn of
  NewTypeCon con -> Just (unLoc con)
  DataTypeCons _ [con] -> Just (unLoc con)
  DataTypeCons _ _ -> Nothing

-- | The one thing a constructor holds, written either way round.
soleField :: ConDecl GhcPs -> Maybe (LHsType GhcPs)
soleField = \case
  ConDeclH98 {con_args = args} -> case args of
    PrefixCon _ [field] -> Just (hsScaledThing field)
    RecCon fields -> case unLoc fields of
      [field] -> case unLoc field of
        ConDeclField {cd_fld_type = t} -> Just t
      _ -> Nothing
    _ -> Nothing
  ConDeclGADT {} -> Nothing

-- | Whether a name says the type is a failure.
--
-- The plural too, because a type that accumulates is named for what it holds.
isErrorName :: Text -> Bool
isErrorName name = T.isSuffixOf "Error" name || T.isSuffixOf "Errors" name
