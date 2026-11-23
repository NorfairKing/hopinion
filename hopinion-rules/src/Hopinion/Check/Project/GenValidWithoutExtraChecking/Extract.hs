{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks which methods an instance leaves to its class.
module Hopinion.Check.Project.GenValidWithoutExtraChecking.Extract
  ( structuralValiditiesOf,
    defaultingGenValidsOf,
  )
where

import qualified Data.List.NonEmpty as NE
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import GHC.Hs
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import Hopinion.Check.Project.GenValidWithoutExtraChecking.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Instance
import Hopinion.Facts.Module
import Hopinion.Parse (ParsedModule, parsedModuleDecls)

-- | Every Validity instance that leaves @validate@ to its default, which
-- validates each field with the field's own instance and checks nothing else.
--
-- An instance that writes @validate@ out is not one, even when it writes the
-- default out: hopinion asks for that to be deleted first.
structuralValiditiesOf :: ModuleContext -> ParsedModule -> [WrittenInstance]
structuralValiditiesOf ctx parsed =
  [ w
  | w <- writtenInstancesOf ctx parsed,
    instanceFactClass (writtenInstanceFact w) == "Validity",
    "validate" `notElem` writtenInstanceMethods w
  ]

-- | Every GenValid instance that leaves a method to its default, with the
-- methods it leaves.
defaultingGenValidsOf :: ModuleContext -> ParsedModule -> [DefaultingGenValid]
defaultingGenValidsOf ctx parsed =
  [ DefaultingGenValid
      { defaultingGenValidInstance = w,
        defaultingGenValidMethods = methods
      }
  | w <- writtenInstancesOf ctx parsed,
    instanceFactClass (writtenInstanceFact w) == "GenValid",
    Just methods <-
      [ NE.nonEmpty
          [ m
          | m <- genValidMethods,
            genValidMethodName m `notElem` writtenInstanceMethods w
          ]
      ]
  ]

-- | Every instance whose methods the source shows.
--
-- An anyclass deriving writes none.
-- A deriving whose strategy is not anyclass, or is not written, can take its
-- methods from another type's instance, so what it does is not in the file.
--
-- The declaration is found by its span, so that which instance it is has the
-- one answer the shared extraction gives.
writtenInstancesOf :: ModuleContext -> ParsedModule -> [WrittenInstance]
writtenInstancesOf ctx parsed =
  [ w
  | inst <- moduleContextInstances ctx,
    Just w <- [writtenInstanceOf ctx parsed inst]
  ]

writtenInstanceOf :: ModuleContext -> ParsedModule -> InstanceFact -> Maybe WrittenInstance
writtenInstanceOf ctx parsed inst =
  let rp = moduleContextPath ctx
      decls :: [HsDecl GhcPs]
      decls =
        [ unLoc ldecl
        | ldecl <- parsedModuleDecls parsed,
          instanceFactSpan inst == spanOfLocated rp ldecl
        ]
      written :: LIdP GhcPs -> LIdP GhcPs -> [Text] -> WrittenInstance
      written cls ty methods =
        WrittenInstance
          { writtenInstanceFact = inst,
            writtenInstanceClassAt = spanOfSrcSpan rp (getLocA cls),
            writtenInstanceTypeAt = spanOfSrcSpan rp (getLocA ty),
            writtenInstanceMethods = methods
          }
   in case instanceFactOrigin inst of
        InstanceOriginInstanceDecl _ ->
          listToMaybe
            [ written cls ty (mapMaybe (bindingName . unLoc) (cid_binds ci))
            | InstD _ (ClsInstD _ ci) <- decls,
              Just (cls, ty) <- [instanceHeadNames (cid_poly_ty ci)]
            ]
        InstanceOriginDerivingAnyclass ->
          listToMaybe
            [ written cls ty []
            | TyClD _ DataDecl {tcdLName = ty, tcdDataDefn = defn} <- decls,
              L _ clause <- dd_derivs defn,
              sigTy <- derivClauseTypes (deriv_clause_tys clause),
              HsSig {sig_body = body} <- [unLoc sigTy],
              Just cls <- [namedHead body],
              rdrText (unLoc cls) == instanceFactClass inst
            ]
        InstanceOriginStandaloneDeriving -> Nothing
        InstanceOriginDerivingStock -> Nothing
        InstanceOriginDerivingNewtype -> Nothing
        InstanceOriginDerivingVia _ -> Nothing
        InstanceOriginDerivingUnspecified -> Nothing

-- | The names of the class and of the type at the head of its argument, in an
-- instance of a class of one argument.
instanceHeadNames :: LHsSigType GhcPs -> Maybe (LIdP GhcPs, LIdP GhcPs)
instanceHeadNames sigTy = case unLoc sigTy of
  HsSig {sig_body = body} -> case unLoc (peelType body) of
    HsAppTy _ cls ty -> (,) <$> namedHead cls <*> namedHead ty
    _ -> Nothing

-- | The method a binding defines.
-- The compiler accepts nothing but function bindings in an instance, so the
-- other kinds never come up.
bindingName :: HsBind GhcPs -> Maybe Text
bindingName = \case
  FunBind {fun_id = name} -> Just (rdrText (unLoc name))
  PatBind {} -> Nothing
  VarBind {} -> Nothing
  PatSynBind {} -> Nothing
