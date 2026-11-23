{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks which synonyms stand for a scalar. What is shared is how a type is taken
-- apart, which comes from 'Hopinion.Extract.Ghc', and which errors are prose,
-- which comes from 'Hopinion.Check.ProseError.Extract'.
module Hopinion.Check.Hs.NewtypeNotSynonym.Extract (scalarSynonymsOf) where

import Data.Maybe (isJust)
import Data.Text (Text)
import GHC.Hs
import GHC.Types.SrcLoc (unLoc)
import Hopinion.Check.Hs.NewtypeNotSynonym.Fact
import Hopinion.Check.ProseError.Extract (errorDeclsOf, textTypeNames)
import Hopinion.Check.ProseError.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Module
import Hopinion.Parse (ParsedModule, parsedModuleDecls)

-- | Every synonym in the module that stands for a scalar.
--
-- Top-level declarations only, because a synonym cannot be declared anywhere
-- else.
--
-- Parameters do not matter: a synonym's right-hand side is what it expands to,
-- so a parameter a scalar does not mention is gone after expansion, and
-- @Key User@ and @Key Order@ are the same type.
--
-- A synonym whose own name is a scalar's is a project naming a library's
-- type, as a prelude does with @type LText = Data.Text.Lazy.Text@, rather than
-- a name for a value.
--
-- An error synonym for prose is left to 'errorDeclsOf', whose rule asks for a
-- sum type instead, so that one declaration is not given two fixes that
-- disagree.
scalarSynonymsOf :: ModuleContext -> ParsedModule -> [ScalarSynonym]
scalarSynonymsOf ctx parsed =
  let rp = moduleContextPath ctx
      ref = moduleContextRef ctx
      decls = moduleContextDecls ctx
      ds = map unLoc (parsedModuleDecls parsed)
      proseErrors =
        [ errorDeclName ed
        | ed <- errorDeclsOf ctx parsed,
          isJust (proseErrorName (errorDeclError ed))
        ]
      ownTypes = [name | TyClD _ d <- ds, Just name <- [ownTypeName d]]
   in [ ScalarSynonym
          { scalarSynonymName = name,
            scalarSynonymScalar = scalar,
            scalarSynonymSpan = sp,
            scalarSynonymScope = declScopeOf ref decls sp
          }
      | ldecl <- parsedModuleDecls parsed,
        TyClD _ SynDecl {tcdLName = n, tcdRhs = rhs} <- [unLoc ldecl],
        let name = rdrText (unLoc n),
        name `notElem` scalarNames,
        name `notElem` proseErrors,
        let sp = spanOfLocated rp ldecl,
        Just scalar <- [scalarOf ownTypes rhs]
      ]

-- | The name of a type the module declares as a type of its own, which is
-- every declaration of a type but a synonym.
--
-- Not a synonym, because a synonym under a scalar's name is a project's name
-- for that scalar, the way a prelude writes one.
ownTypeName :: TyClDecl GhcPs -> Maybe Text
ownTypeName = \case
  SynDecl {} -> Nothing
  d -> Just (rdrText (unLoc (tyClDeclLName d)))

-- | The scalar a type is, if it is one, by name.
--
-- @[Char]@ is @String@ spelled out, so it answers as @String@.
--
-- Qualified or not: 'peelApp' answers with the occurrence name, so
-- @Data.Text.Lazy.Text@ arrives here as @Text@.
--
-- A name the module declares a type of its own under is that type rather than
-- the scalar, so it is not one.
scalarOf :: [Text] -> LHsType GhcPs -> Maybe Text
scalarOf ownTypes lt =
  let isScalarName :: Text -> Bool
      isScalarName name = name `notElem` ownTypes
   in case unLoc (peelType lt) of
        HsListTy _ el -> case peelApp el of
          Just ("Char", []) | isScalarName "Char" -> Just "String"
          _ -> Nothing
        _ -> case peelApp lt of
          Just (name, []) | name `elem` scalarNames, isScalarName name -> Just name
          _ -> Nothing

-- | The types a domain value is written as when nobody gave it a type.
--
-- A list rather than a question put to the compiler, because what makes these
-- scalars is that a reader recognises them on sight as carrying no meaning of
-- their own.
--
-- Two are left out on purpose. @FilePath@ is the business of the rule that asks
-- for a @Path@ instead. @Bool@ is better replaced by a type with two named
-- constructors than wrapped, so a newtype would be the wrong fix to offer.
scalarNames :: [Text]
scalarNames =
  textTypeNames
    ++ [ "String",
         "Char",
         "ShortText",
         "ByteString",
         "LByteString",
         "StrictByteString",
         "LazyByteString",
         "ShortByteString",
         "Int",
         "Int8",
         "Int16",
         "Int32",
         "Int64",
         "Integer",
         "Natural",
         "Word",
         "Word8",
         "Word16",
         "Word32",
         "Word64",
         "Double",
         "Float",
         "Rational",
         "Scientific",
         "Uni",
         "Deci",
         "Centi",
         "Milli",
         "Micro",
         "Nano",
         "Pico",
         "UUID"
       ]
