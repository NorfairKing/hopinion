{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks what a constructor holds in a position. What is shared is how a type is
-- taken apart, and that comes from 'Hopinion.Extract.Ghc'.
module Hopinion.Check.Hs.NoAnonymousScalar.Extract (anonymousScalarsOf) where

import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Hs
import GHC.Types.SrcLoc (unLoc)
import Hopinion.Check.Hs.NoAnonymousScalar.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Decl
import Hopinion.Facts.Place
import Hopinion.Facts.Primitive (isPrimitiveTypeName)
import Path (File, Path, Rel)

-- | Every constructor in the module that holds an anonymous primitive its own
-- name cannot speak for.
--
-- A constructor's name speaks for one payload, so a constructor of one field
-- needs nothing: the name is that field's name. A constructor of several names
-- the whole of what it holds and no part of it, so every primitive among them
-- is anonymous and every one is reported.
--
-- Over the data definitions rather than over the type declarations, so that a
-- constructor a data family instance writes is read as well: it is positional
-- the same way, and whoever passes arguments to it counts them the same way.
anonymousScalarsOf :: Path Rel File -> ModuleRef -> [DeclFact] -> [LHsDecl GhcPs] -> [AnonymousScalars]
anonymousScalarsOf rp ref decls ds =
  [ AnonymousScalars
      { anonymousScalarsConstructor = con,
        anonymousScalarsFields = fromIntegral (length fields),
        anonymousScalarsTypes = primitives,
        anonymousScalarsSpan = sp,
        anonymousScalarsScope = declScopeOf ref decls sp
      }
  | defn <- declaredDataDefns ds,
    lcon <- toList (dd_cons defn),
    let sp = spanOfSrcSpan rp (getLocA lcon),
    (con, fields) <- positionallyHeldBy (unLoc lcon),
    length fields > 1,
    let primitives = mapMaybe anonymousPrimitiveOf fields,
    not (null primitives)
  ]

-- | Every data definition the module declares, and none that it only quotes.
--
-- Written as a walk over the declarations rather than as a search through them,
-- because a search finds the constructors inside a Template Haskell bracket as
-- well, and those are a template rather than a declaration: nothing in the
-- module has such a constructor, a reader never passes it an argument, and the
-- fix a finding asks for lands on a bracket. What a splice writes is beyond
-- this rule either way, so a quote is the half of that it can decline to read.
--
-- A data family instance is read wherever it is written, inside a class
-- instance or beside one, because the constructor it writes is positional the
-- same way.
declaredDataDefns :: [LHsDecl GhcPs] -> [HsDataDefn GhcPs]
declaredDataDefns =
  concatMap
    ( ( \case
          TyClD _ DataDecl {tcdDataDefn = defn} -> [defn]
          InstD _ (DataFamInstD _ dfid) -> [dataFamInstDefn dfid]
          InstD _ (ClsInstD _ ClsInstDecl {cid_datafam_insts = insts}) ->
            map (dataFamInstDefn . unLoc) insts
          _ -> []
      )
        . unLoc
    )

dataFamInstDefn :: DataFamInstDecl GhcPs -> HsDataDefn GhcPs
dataFamInstDefn = feqn_rhs . dfid_eqn

-- | What each name one constructor declaration introduces holds in a position.
--
-- A name at a time rather than the declaration at a time, because a GADT may
-- write several constructors over one argument list, and a reader meets each
-- of them on its own.
positionallyHeldBy :: ConDecl GhcPs -> [(Text, [LHsType GhcPs])]
positionallyHeldBy = \case
  ConDeclH98 {con_name = n, con_args = args} ->
    [(rdrText (unLoc n), h98Positional args)]
  ConDeclGADT {con_names = ns, con_g_args = args} ->
    [(rdrText (unLoc n), gadtPositional args) | n <- toList ns]

-- | An infix constructor is among these, and holds its two fields as
-- positionally as a prefix one: its name is written between them rather than
-- on them.
h98Positional :: HsConDeclH98Details GhcPs -> [LHsType GhcPs]
h98Positional = \case
  PrefixCon _ args -> map hsScaledThing args
  InfixCon a b -> map hsScaledThing [a, b]
  -- A record holds nothing in a position: every field it has is named, which
  -- is the other half of what this rule asks for.
  RecCon _ -> []

gadtPositional :: HsConDeclGADTDetails GhcPs -> [LHsType GhcPs]
gadtPositional = \case
  PrefixConGADT _ args -> map hsScaledThing args
  RecConGADT _ _ -> []

-- | The primitive one field holds, under the name a reader meets it by, or
-- nothing when the field is not an anonymous primitive at all.
--
-- A @Maybe@ of a primitive is one, and is named as it is written, because a
-- reader of @!(Maybe Text)@ has been told whether there is one and nothing
-- else. A field whose type takes arguments of its own is not one however
-- primitive what it takes may be, since a type that says something is a name.
anonymousPrimitiveOf :: LHsType GhcPs -> Maybe Text
anonymousPrimitiveOf lt = do
  (con, args) <- appliedTypeOf lt
  case args of
    [] | isPrimitiveTypeName con -> Just con
    -- A list and a NonEmpty are collections rather than the thing they
    -- collect, and a reader of one has been told that there are several, which
    -- is something. The exception is @[Char]@: that is @String@ spelled out,
    -- and a primitive this rule read under one spelling and not the other
    -- would be a way around it that no other rule closes.
    [arg] | con == "[]", anonymousPrimitiveOf arg == Just "Char" -> Just "[Char]"
    -- A Maybe holds at most one of the thing, so a field holding one is as
    -- anonymous as a field holding the thing, and the newtype that fixes it
    -- goes inside.
    [arg] | con == "Maybe" -> appliedText con <$> anonymousPrimitiveOf arg
    _ -> Nothing

-- | The head of a type and the arguments it is applied to, with a list read as
-- the application it stands for.
--
-- 'peelApp' answers for a list with the arguments the list itself was given,
-- because what a list is written over is syntax rather than an argument to a
-- constructor that could take anything else. Here the element is the whole
-- question, so the syntax is read as the application a reader reads it as.
appliedTypeOf :: LHsType GhcPs -> Maybe (Text, [LHsType GhcPs])
appliedTypeOf lt = case unLoc (peelType lt) of
  HsListTy _ el -> Just ("[]", [el])
  _ -> peelApp lt

-- | A one-argument application written the way its own syntax writes it.
--
-- An argument that is itself an application is parenthesised, because what
-- this builds is read beside the code it is about, and a reader of @Maybe Maybe
-- Text@ would be looking for a type their file does not hold.
appliedText :: Text -> Text -> Text
appliedText con inner
  | T.isInfixOf " " inner = T.concat [con, " (", inner, ")"]
  | otherwise = T.concat [con, " ", inner]
