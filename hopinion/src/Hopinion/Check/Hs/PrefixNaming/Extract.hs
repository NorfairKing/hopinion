{-# LANGUAGE LambdaCase #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks what a data declaration calls the names it introduces. What is shared is
-- how a name is read off the tree, and that comes from 'Hopinion.Extract.Ghc'.
module Hopinion.Check.Hs.PrefixNaming.Extract (unprefixedNamesOf) where

import Data.Char (isAlpha)
import Data.Foldable (toList)
import Data.Generics (listify)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Hs
import GHC.Types.SrcLoc (unLoc)
import Hopinion.Check.Hs.PrefixNaming.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Decl
import Hopinion.Facts.Name
import Hopinion.Facts.Place
import Path (File, Path, Rel)

-- | Every constructor and every field in the module whose name does not start
-- with the name of the type it belongs to.
--
-- Over the type declarations, which is every @data@ and @newtype@ and nothing
-- else. An instance of a data family is not one of them and is left alone: its
-- names belong to the family rather than to the head written above them, so
-- which of the two a prefix would have to carry has no one answer.
--
-- A name that does not begin with a letter is left alone, in either role, which
-- is two kinds of name. An infix constructor is read at its use site as the
-- operator between its arguments rather than as a word, so there is nowhere in
-- it for a prefix to go. A field opening with an underscore is spelled that way
-- to say it is the one behind a lens, and the name the lens takes from it is
-- what carries the type, so holding the field itself to the prefix would refuse
-- the spelling that convention asks for.
unprefixedNamesOf :: Path Rel File -> ModuleRef -> [DeclFact] -> [LHsDecl GhcPs] -> [UnprefixedName]
unprefixedNamesOf rp ref decls ds =
  [ UnprefixedName
      { unprefixedNameRole = role,
        unprefixedNameWritten = written,
        unprefixedNameType = th,
        unprefixedNameSpan = sp,
        unprefixedNameScope = declScopeOf ref decls sp
      }
  | DataDecl {tcdLName = n, tcdDataDefn = defn} <- listify (const True :: TyClDecl GhcPs -> Bool) ds,
    let th = TypeHead (rdrText (unLoc n)),
    isWord (typeHeadText th),
    (role, written, sp) <- introducedBy rp defn,
    isWord written,
    not (prefixedByType th written)
  ]

-- | A name opening with a letter, which is the only kind a prefix can be read
-- off the front of.
isWord :: Text -> Bool
isWord t = maybe False (isAlpha . fst) (T.uncons t)

-- | Every name the right-hand side of a data declaration introduces.
introducedBy :: Path Rel File -> HsDataDefn GhcPs -> [(NameRole, Text, Span)]
introducedBy rp defn = concatMap (namesOfCon rp . unLoc) (dd_cons defn)

namesOfCon :: Path Rel File -> ConDecl GhcPs -> [(NameRole, Text, Span)]
namesOfCon rp = \case
  ConDeclH98 {con_name = n, con_args = args} ->
    constructorNameAt rp n : concatMap (namesOfField rp) (h98Fields args)
  ConDeclGADT {con_names = ns, con_g_args = args} ->
    map (constructorNameAt rp) (toList ns) ++ concatMap (namesOfField rp) (gadtFields args)

constructorNameAt :: Path Rel File -> LIdP GhcPs -> (NameRole, Text, Span)
constructorNameAt rp ln = (NameRoleConstructor, rdrText (unLoc ln), spanOfSrcSpan rp (getLocA ln))

h98Fields :: HsConDeclH98Details GhcPs -> [LConDeclField GhcPs]
h98Fields = \case
  RecCon lfs -> unLoc lfs
  PrefixCon _ _ -> []
  InfixCon _ _ -> []

gadtFields :: HsConDeclGADTDetails GhcPs -> [LConDeclField GhcPs]
gadtFields = \case
  RecConGADT _ lfs -> unLoc lfs
  PrefixConGADT _ _ -> []

-- | One declared field can name several selectors, each of which is a name of
-- its own to get right.
namesOfField :: Path Rel File -> LConDeclField GhcPs -> [(NameRole, Text, Span)]
namesOfField rp lf =
  [ (NameRoleField, rdrText (unLoc (foLabel (unLoc lfo))), spanOfSrcSpan rp (getLocA lfo))
  | lfo <- cd_fld_names (unLoc lf)
  ]
