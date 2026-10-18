{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks what a declaration wraps or what a field is written as. What is shared
-- is how a type is taken apart, and that comes from 'Hopinion.Extract.Ghc'.
module Hopinion.Check.Project.ScalarNewtypeUnused.Extract
  ( scalarNewtypesOf,
    primitiveFieldsOf,
  )
where

import GHC.Hs
import GHC.Types.SrcLoc (SrcSpan, unLoc)
import Hopinion.Check.Project.ScalarNewtypeUnused.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Name
import Hopinion.Facts.Primitive (Primitive, primitiveNamed)
import Path (File, Path, Rel)

-- | Every type the module declares whose values are each one primitive.
--
-- Written as a @newtype@ or as a @data@ with one constructor, and holding its
-- primitive positionally or under a field name: all four spellings say the same
-- thing about the values of the type, and a rule reading only one of them would
-- be a rule about how the declaration is written.
--
-- Over the type declarations, which is every @data@ and @newtype@ and nothing
-- else. An instance of a data family is not one of them and is left alone: what
-- it declares belongs to the family rather than to the head written above it.
scalarNewtypesOf :: [LHsDecl GhcPs] -> [ScalarNewtype]
scalarNewtypesOf ds =
  [ ScalarNewtype
      { scalarNewtypeType = TypeHead (rdrText (unLoc n)),
        scalarNewtypePrimitive = prim
      }
  | DataDecl {tcdLName = n, tcdDataDefn = defn} <- declaredDataDecls ds,
    Just con <- [soleConstructor defn],
    Just wrapped <- [soleField con],
    Just prim <- [primitiveWrittenAs wrapped]
  ]

-- | Every record field the module declares that is written as a bare
-- primitive.
--
-- One fact per selector, since a field naming several of them is several names
-- to get right and the type under each of them is the same one.
primitiveFieldsOf :: Path Rel File -> [LHsDecl GhcPs] -> [PrimitiveField]
primitiveFieldsOf rp ds =
  [ PrimitiveField
      { primitiveFieldOwner = TypeHead (rdrText (unLoc n)),
        primitiveFieldName = rdrText (unLoc (foLabel (unLoc lfo))),
        primitiveFieldPrimitive = prim,
        primitiveFieldSpan = spanOfSrcSpan rp at
      }
  | DataDecl {tcdLName = n, tcdDataDefn = defn} <- declaredDataDecls ds,
    lf <- concatMap recordFieldsOf (constructorsOf defn),
    Just (prim, at) <- [primitiveUnderContainers (cd_fld_type (unLoc lf))],
    lfo <- cd_fld_names (unLoc lf)
  ]

-- | Every @data@ and @newtype@ the module declares, and none that it only
-- quotes.
--
-- Written as a walk over the declarations rather than as a search through them,
-- because a search finds the ones inside a Template Haskell bracket as well. A
-- quoted newtype is a template: the module declares no such type, nothing can
-- import it, and reading one into the lexicon would have this rule name a type
-- to use that does not exist. That is the same reason it concludes only from
-- what is written, and a bracket is written for a splice rather than for a
-- reader.
--
-- An instance of a data family is not here for a different reason, which is
-- that what it declares belongs to the family rather than to the head written
-- above it.
declaredDataDecls :: [LHsDecl GhcPs] -> [TyClDecl GhcPs]
declaredDataDecls ds =
  [ d
  | TyClD _ d@DataDecl {} <- map unLoc ds
  ]

-- | The primitive written here with nothing in front of it, which is what a
-- scalar newtype wraps.
--
-- A collection of primitives is not one: a type holding several of something
-- is not a scalar, whatever the something is.
primitiveWrittenAs :: LHsType GhcPs -> Maybe Primitive
primitiveWrittenAs lt =
  let peeled = peelType lt
   in case unLoc peeled of
        -- @[Char]@ is @String@ spelled out, and a list is the one place a
        -- @Char@ is prose rather than a character.
        HsListTy _ el | isCharType el -> primitiveNamed "String"
        _ -> case peelApp peeled of
          Just (name, []) -> primitiveNamed name
          _ -> Nothing

-- | The same, seen through the containers that do not change what a type is
-- about, and where that primitive is written.
--
-- @Maybe@, a list and @NonEmpty@ are transparent, because a @Maybe Text@ whose
-- @Text@ is a @Username@ is a @Maybe Username@: the name goes inside the
-- container and the container is nobody's business. Measured both ways round
-- before being written this way: reading through them adds findings and every
-- one of them was real.
primitiveUnderContainers :: LHsType GhcPs -> Maybe (Primitive, SrcSpan)
primitiveUnderContainers lt =
  let peeled = peelType lt
   in case primitiveWrittenAs peeled of
        Just prim -> Just (prim, getLocA peeled)
        Nothing -> case unLoc peeled of
          HsListTy _ el -> primitiveUnderContainers el
          _ -> case peelApp peeled of
            Just ("Maybe", [el]) -> primitiveUnderContainers el
            Just ("NonEmpty", [el]) -> primitiveUnderContainers el
            _ -> Nothing

isCharType :: LHsType GhcPs -> Bool
isCharType el = case peelApp el of
  Just ("Char", []) -> True
  _ -> False

constructorsOf :: HsDataDefn GhcPs -> [ConDecl GhcPs]
constructorsOf defn = case dd_cons defn of
  NewTypeCon con -> [unLoc con]
  DataTypeCons _ cons -> map unLoc cons

soleConstructor :: HsDataDefn GhcPs -> Maybe (ConDecl GhcPs)
soleConstructor defn = case constructorsOf defn of
  [con] -> Just con
  _ -> Nothing

-- | The one thing a constructor holds, written either way round, and nothing
-- for a constructor holding anything else.
soleField :: ConDecl GhcPs -> Maybe (LHsType GhcPs)
soleField = \case
  ConDeclH98 {con_args = args} -> case args of
    PrefixCon _ [field] -> Just (hsScaledThing field)
    PrefixCon _ _ -> Nothing
    RecCon lfs -> case unLoc lfs of
      [lf] -> case cd_fld_names (unLoc lf) of
        [_] -> Just (cd_fld_type (unLoc lf))
        _ -> Nothing
      _ -> Nothing
    InfixCon _ _ -> Nothing
  ConDeclGADT {} -> Nothing

recordFieldsOf :: ConDecl GhcPs -> [LConDeclField GhcPs]
recordFieldsOf = \case
  ConDeclH98 {con_args = RecCon lfs} -> unLoc lfs
  ConDeclH98 {} -> []
  ConDeclGADT {} -> []
