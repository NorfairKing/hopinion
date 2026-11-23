{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Which declarations name an error that is prose, and which types are prose.
--
-- In the core because more than one rule asks, and the rules have to agree:
-- an error synonym one of them reports is one the other leaves alone.
module Hopinion.Check.ProseError.Extract
  ( errorDeclsOf,
    errorTypeOf,
    textTypeNames,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Hs
import GHC.Types.SrcLoc (unLoc)
import Hopinion.Check.ProseError.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Module
import Hopinion.Parse (ParsedModule, parsedModuleDecls)

-- | Every declaration that calls itself an error and is prose underneath.
--
-- Read off the name, which is the one thing that says a type is meant as a
-- failure. That misses an error type named well, and catches the one written to
-- get out of the way of a rule: a signature reported for its @String@ is
-- answered by naming the @String@ and nothing else.
errorDeclsOf :: ModuleContext -> ParsedModule -> [ErrorDecl]
errorDeclsOf ctx parsed =
  let rp = moduleContextPath ctx
      ref = moduleContextRef ctx
      decls = moduleContextDecls ctx
      ds = parsedModuleDecls parsed
   in [ ErrorDecl
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

-- | The names a @Text@ is written under, strict or lazy.
--
-- Qualified or not: 'peelApp' answers with the occurrence name, so
-- @Data.Text.Lazy.Text@ is read as @Text@. The other names are the ones the
-- @text@ package exports for each variant, and the one a project prelude gives
-- the lazy one.
textTypeNames :: [Text]
textTypeNames = ["Text", "StrictText", "LazyText", "LText"]

-- | What an error is, seen through the containers that hold several of them.
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
    Just (name, []) | name `elem` textTypeNames -> ErrorTypeText
    _ -> ErrorTypeSomethingElse

-- | @[Char]@ is @String@ spelled out, and a list is the one place a @Char@ is
-- prose. On its own it is a character, which no error message is.
elementErrorType :: LHsType GhcPs -> ErrorType
elementErrorType el = case peelApp el of
  Just ("Char", []) -> ErrorTypeString
  _ -> errorTypeOf el

-- | The name a declaration gives itself, how it is written, and the type it is
-- prose in if it is prose at all.
--
-- A data declaration answers only when its one constructor holds one thing and
-- is the type's own name again, which is the wrapper a reader learns nothing
-- from. A @data@ with two constructors is the fix that is asked for.
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
