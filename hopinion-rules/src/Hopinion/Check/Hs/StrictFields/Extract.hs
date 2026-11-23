{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks how strict a field is.
module Hopinion.Check.Hs.StrictFields.Extract (lazyFieldsOf) where

import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Hs
import qualified GHC.LanguageExtensions as LangExt
import GHC.Types.SrcLoc (unLoc)
import Hopinion.Check.Hs.StrictFields.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Module
import Hopinion.Facts.Place
import Hopinion.Parse (ParsedModule (..))
import Path (File, Path, Rel)

-- | Every field of a data declaration in the module that is lazy.
--
-- Over the data definitions rather than the declarations, because a data
-- family instance has fields to make strict as well, and both are one
-- definition.
-- A newtype has no field to annotate, since its constructor is erased.
-- The constructors of a @type data@ declaration exist only in types, so they
-- have no fields at run time to be lazy in.
lazyFieldsOf :: ModuleContext -> ParsedModule -> [HsDataDefn GhcPs] -> [LazyField]
lazyFieldsOf ctx parsed defns =
  let rp = moduleContextPath ctx
      ref = moduleContextRef ctx
      decls = moduleContextDecls ctx
      unmarked = unmarkedFieldsIn parsed
   in [ LazyField
          { lazyFieldName = fieldName field,
            lazyFieldSpan = fieldSpan field,
            lazyFieldScope = declScopeOf ref decls (fieldSpan field)
          }
      | defn <- defns,
        DataTypeCons False lcons <- [dd_cons defn],
        field <- concatMap (constructorFieldsOf rp) lcons,
        isLazy unmarked field
      ]

-- | What a field with no strictness annotation is.
data Unmarked
  = UnmarkedLazy
  | UnmarkedStrict

-- | @StrictData@ alone, because it is the only extension GHC asks when it
-- decides how strict a field is: @Strict@ makes fields strict only by implying
-- it, and a module can turn @StrictData@ back off while keeping @Strict@.
unmarkedFieldsIn :: ParsedModule -> Unmarked
unmarkedFieldsIn parsed =
  if EnumSet.member LangExt.StrictData (parsedModuleExtensions parsed)
    then UnmarkedStrict
    else UnmarkedLazy

-- | A @~@ makes a field lazy whatever the extensions say.
-- A field of an unlifted type is never a thunk, and GHC warns about a @!@ on
-- one, so it is never lazy, whatever is written.
isLazy :: Unmarked -> Field -> Bool
isLazy unmarked field = case fieldLiftedness field of
  LiftednessUnlifted -> False
  LiftednessLifted -> case fieldStrictness field of
    SrcLazy -> True
    SrcStrict -> False
    NoSrcStrict -> case unmarked of
      UnmarkedLazy -> True
      UnmarkedStrict -> False

data Field = Field
  { fieldName :: !LazyFieldName,
    fieldSpan :: !Span,
    fieldStrictness :: !SrcStrictness,
    fieldLiftedness :: !Liftedness
  }

-- | Whether a field's type has values that can be thunks, as far as its
-- syntax tells: an unboxed tuple or sum is unlifted, and so, by convention, is
-- a type whose name ends in @#@.
data Liftedness
  = LiftednessLifted
  | LiftednessUnlifted

-- | A GADT signature can declare several constructors at once, and a
-- positional field is named after the first of them.
constructorFieldsOf :: Path Rel File -> LConDecl GhcPs -> [Field]
constructorFieldsOf rp lcon =
  let (name, written) = case unLoc lcon of
        ConDeclH98 {con_name = n, con_args = args} -> (n, h98Fields args)
        ConDeclGADT {con_names = ns, con_g_args = args} -> (NE.head ns, gadtFields args)
   in fieldsWith rp (rdrText (unLoc name)) written

-- | A constructor's fields, as records or as positions.
data Written
  = WrittenRecord ![LConDeclField GhcPs]
  | WrittenPositional ![LBangType GhcPs]

h98Fields :: HsConDeclH98Details GhcPs -> Written
h98Fields = \case
  RecCon lfs -> WrittenRecord (unLoc lfs)
  PrefixCon _ args -> WrittenPositional (map hsScaledThing args)
  InfixCon l r -> WrittenPositional [hsScaledThing l, hsScaledThing r]

gadtFields :: HsConDeclGADTDetails GhcPs -> Written
gadtFields = \case
  RecConGADT _ lfs -> WrittenRecord (unLoc lfs)
  PrefixConGADT _ args -> WrittenPositional (map hsScaledThing args)

fieldsWith :: Path Rel File -> T.Text -> Written -> [Field]
fieldsWith rp constructor = \case
  WrittenRecord lfs -> mapMaybe (recordField rp) lfs
  WrittenPositional tys ->
    [ Field
        { fieldName = LazyFieldNamePositional constructor position,
          fieldSpan = spanOfSrcSpan rp (getLocA ty),
          fieldStrictness = strictnessOf ty,
          fieldLiftedness = liftednessOf ty
        }
    | (position, ty) <- zip [1 ..] tys
    ]

-- | One declared field, however many names it gives its type.
recordField :: Path Rel File -> LConDeclField GhcPs -> Maybe Field
recordField rp lf = do
  let field = unLoc lf
  names <- NE.nonEmpty (map (rdrText . unLoc . foLabel . unLoc) (cd_fld_names field))
  pure
    Field
      { fieldName = LazyFieldNameRecord names,
        fieldSpan = spanOfSrcSpan rp (getLocA lf),
        fieldStrictness = strictnessOf (cd_fld_type field),
        fieldLiftedness = liftednessOf (cd_fld_type field)
      }

-- | A documented type carries its annotation inside the documentation.
strictnessOf :: LBangType GhcPs -> SrcStrictness
strictnessOf lt = case unLoc lt of
  HsBangTy _ (HsBang _ strictness) _ -> strictness
  HsDocTy _ inner _ -> strictnessOf inner
  _ -> NoSrcStrict

liftednessOf :: LBangType GhcPs -> Liftedness
liftednessOf lt = case unLoc (peelType lt) of
  HsTupleTy _ HsUnboxedTuple _ -> LiftednessUnlifted
  HsSumTy {} -> LiftednessUnlifted
  _ -> case peelApp lt of
    Just (name, _) | "#" `T.isSuffixOf` name -> LiftednessUnlifted
    _ -> LiftednessLifted
