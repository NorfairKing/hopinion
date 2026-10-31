{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | What the compiler wrote down about a module: the @.hie@ file for what it
-- names, the @.hi@ interface for what it declares.
--
-- Its own package because these are read by the @ghc@ library and hopinion
-- parses with @ghc-lib-parser@; both expose modules under @GHC.@, so one
-- component cannot import both. Only plain data crosses the boundary.
--
-- The version coupling that leaves is real and contained: the reader has to be
-- built by the compiler that built the code under check, which the Nix layer
-- arranges by taking both from one package set.
module Hopinion.Hie
  ( CompiledModule (..),
    ArtifactUnreadable (..),
    renderArtifactUnreadable,
    readCompiledModule,
    DeclaredInstance (..),
    readDeclaredInstances,
    HiePosition (..),
    NameOrigin (..),
    readNameOrigins,
  )
where

import Control.Exception (SomeException, displayException, evaluate, try)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Path ()
import Data.Validity.Text ()
import GHC.Generics (Generic)
import GHC.Iface.Binary (CheckHiWay (..), TraceBinIFace (..), readBinIface)
import GHC.Iface.Ext.Binary (hie_file_result, readHieFile)
import GHC.Iface.Ext.Types
  ( ContextInfo (..),
    HieAST (..),
    HieFile (..),
    IdentifierDetails (..),
    NodeInfo (..),
    getAsts,
    getSourcedNodeInfo,
    hie_asts,
  )
import GHC.Iface.Syntax (IfaceClsInst (..))
import GHC.Iface.Type (IfaceTyCon (..))
import GHC.Platform (genericPlatform)
import GHC.Platform.Profile (Profile (..))
import GHC.Types.Name (nameModule_maybe, nameOccName)
import GHC.Types.Name.Cache (initNameCache)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.SrcLoc (srcSpanStartCol, srcSpanStartLine)
import GHC.Unit.Module (moduleName, moduleNameString, moduleUnit)
import GHC.Unit.Module.ModIface (mi_insts)
import GHC.Unit.Types (unitString)
import Path (Abs, File, Path, Rel, parseRelFile, toFilePath)

-- | What one module's @.hie@ file is worth to a rule, which is what its
-- generated code named and nothing else.
--
-- A @.hie@ file keeps no shape for generated code, so which class went with
-- which type comes from the interface instead.
--
-- Ordinary code's names are not carried, because the parser already read them.
-- This is the seam where more of the file would arrive: it also holds the type
-- of every node, which the rules no parser can reach will need.
data CompiledModule = CompiledModule
  { compiledModuleName :: !Text,
    -- | The source file GHC compiled, as GHC was given it: relative to the
    -- package, not the repository. Here because a module name does not identify
    -- a module, and two @Main@s are told apart by this and nothing else.
    --
    -- Nothing when GHC recorded something that is not a package-relative file,
    -- an absolute path included. Such a build says nothing that tells its own
    -- modules apart, so whatever matches a module against one of these has
    -- nothing to match on and must match none rather than guess.
    compiledModuleFile :: !(Maybe (Path Rel File)),
    -- | One set of names per splice expansion.
    --
    -- Generated code arrives as one node carrying the union of every name it
    -- produced, where ordinary code spreads its names across a leaf each. So a
    -- node naming two things is the only place two names can be known to have
    -- arrived together, which is what @genValidSpec \@T@ needs and a
    -- module-wide name set cannot give.
    compiledModuleGenerated :: ![Set Text]
  }
  deriving (Show, Eq, Generic)

instance Validity CompiledModule

-- | Why a file the compiler was supposed to have written could not be read.
--
-- One constructor, because there is one answer to give: the reader threw, and
-- what it threw is GHC's own words about a binary format this build does not
-- speak. Named rather than left as a bare 'Text' so that a caller has to
-- render it before showing it to anybody.
newtype ArtifactUnreadable = ArtifactUnreadable Text
  deriving (Show, Eq)

renderArtifactUnreadable :: ArtifactUnreadable -> Text
renderArtifactUnreadable (ArtifactUnreadable t) = t

-- | Read one @.hie@ file, or say why it could not be read.
--
-- A file written by another compiler is the expected failure, reported rather
-- than thrown so the caller can decide what knowing nothing means.
readCompiledModule :: Path Abs File -> IO (Either ArtifactUnreadable CompiledModule)
readCompiledModule path = do
  result <- try $ do
    cache <- initNameCache 'z' []
    file <- hie_file_result <$> readHieFile cache (toFilePath path)
    let asts = M.elems (getAsts (hie_asts file))
    let compiled =
          CompiledModule
            { compiledModuleName = T.pack (moduleNameString (moduleName (hie_module file))),
              compiledModuleFile = parseRelFile (hie_hs_file file),
              compiledModuleGenerated = concatMap generatedOf asts
            }
    -- Forced inside the catch, so a file this compiler cannot read fails here
    -- rather than as an exception in the middle of a rule.
    _ <- evaluate (length (compiledModuleGenerated compiled))
    pure compiled
  pure $ case result of
    Left (e :: SomeException) -> Left (reason e)
    Right compiled -> Right compiled

-- | Why a file could not be read, in the one line that says it. The rest is a
-- backtrace through this reader, which the person reading the report cannot act
-- on.
reason :: SomeException -> ArtifactUnreadable
reason = ArtifactUnreadable . T.takeWhile (/= '\n') . T.pack . displayException

namesAt :: HieAST a -> Set Text
namesAt ast =
  S.fromList
    [ T.pack (occNameString (nameOccName n))
    | info <- M.elems (getSourcedNodeInfo (sourcedNodeInfo ast)),
      Right n <- M.keys (nodeIdentifiers info)
    ]

-- | The name sets of the nodes that name more than one thing at once, which is
-- what 'compiledModuleGenerated' holds and says why.
--
-- Every node rather than the childless ones: a declaration splice collapses to
-- a leaf and an expression splice keeps children, so asking for leaves found
-- every generated instance and no generated call.
generatedOf :: HieAST a -> [Set Text]
generatedOf ast =
  [names | let { names = namesAt ast }, S.size names > 1] ++ concatMap generatedOf (nodeChildren ast)

-- | One instance a module declares, however it came to declare it.
--
-- An interface records what a module defines whether it was written, derived or
-- spliced, and records them the same way, which is what makes a splice
-- answerable.
--
-- The type is the head's constructor and nothing else: @GenValid (Set Int)@ is
-- an instance for @Set@, which is the lossiness
-- 'Hopinion.Facts.Name.TypeHead' has on the source side and for its reason.
data DeclaredInstance = DeclaredInstance
  { declaredInstanceClass :: !Text,
    declaredInstanceType :: !Text
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity DeclaredInstance

-- | Every instance one module declares, read out of its @.hi@ interface, or why
-- it could not be read.
--
-- A generic platform, no ways and an ignored way tag, because this reads the
-- instance list and nothing that depends on the flavour of the build.
readDeclaredInstances :: Path Abs File -> IO (Either ArtifactUnreadable [DeclaredInstance])
readDeclaredInstances path = do
  result <- try $ do
    cache <- initNameCache 'z' []
    iface <- readBinIface profile cache IgnoreHiWay QuietBinIFace (toFilePath path)
    let instances = mapMaybe declaredInstanceOf (mi_insts iface)
    -- Forced inside the catch, for the reason the .hie read is.
    _ <- evaluate (length instances)
    pure instances
  pure $ case result of
    Left (e :: SomeException) -> Left (reason e)
    Right instances -> Right instances
  where
    profile =
      Profile
        { profilePlatform = genericPlatform,
          profileWays = mempty
        }

-- | An instance whose head is not a type constructor is not one an obligation
-- can be about, since there is no type to write @genValidSpec \@T@ at.
--
-- The last head type rather than the first: an interface records the invisible
-- arguments too, so a poly-kinded class has its kind in front and arrives as
-- @[TYPE, Widget]@ where an ordinary one is @[Written]@. Measured, not read
-- from the manual.
declaredInstanceOf :: IfaceClsInst -> Maybe DeclaredInstance
declaredInstanceOf inst = case reverse (ifInstTys inst) of
  (Just tycon : _) ->
    Just
      DeclaredInstance
        { declaredInstanceClass = T.pack (occNameString (nameOccName (ifInstCls inst))),
          declaredInstanceType = T.pack (occNameString (nameOccName (ifaceTyConName tycon)))
        }
  _ -> Nothing

-- | Where a name is written, as the line and column it starts at.
--
-- The start alone, because an occurrence of a name is one token: where it
-- starts is where it is, and what a parser reading the same file reports for
-- that token starts there too.
data HiePosition = HiePosition
  { hiePositionLine :: !Word,
    hiePositionCol :: !Word
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity HiePosition

-- | Where the thing a name refers to is defined.
--
-- This module, for anything it defines itself, top-level or local, and
-- otherwise the unit the defining module was compiled into, by its id. Which
-- of those units are one's own project is not something a @.hie@ file knows,
-- so it is left to whoever does.
data NameOrigin
  = NameOriginThisModule
  | NameOriginUnit !Text
  deriving (Show, Eq, Generic)

instance Validity NameOrigin

-- | What every name written in one module refers to, by where it is written,
-- or why the @.hie@ file could not be read.
readNameOrigins :: Path Abs File -> IO (Either ArtifactUnreadable (M.Map HiePosition NameOrigin))
readNameOrigins path = do
  result <- try $ do
    cache <- initNameCache 'z' []
    file <- hie_file_result <$> readHieFile cache (toFilePath path)
    let here = hie_module file
    let originOf n = case nameModule_maybe n of
          Nothing -> NameOriginThisModule
          Just m
            | m == here -> NameOriginThisModule
            | otherwise -> NameOriginUnit (T.pack (unitString (moduleUnit m)))
    let origins =
          M.fromListWith
            const
            [ ( HiePosition
                  { hiePositionLine = intToWord (srcSpanStartLine (nodeSpan ast)),
                    hiePositionCol = intToWord (srcSpanStartCol (nodeSpan ast))
                  },
                originOf n
              )
            | top <- M.elems (getAsts (hie_asts file)),
              ast <- nodesOf top,
              info <- M.elems (getSourcedNodeInfo (sourcedNodeInfo ast)),
              (Right n, details) <- M.toList (nodeIdentifiers info),
              any isWritten (identInfo details)
            ]
    -- Forced inside the catch, for the reason the other reads are.
    _ <- evaluate (M.size origins)
    pure origins
  pure $ case result of
    Left (e :: SomeException) -> Left (reason e)
    Right origins -> Right origins
  where
    -- A name the source writes, which is a use or a field it sets. The
    -- compiler also records names nobody wrote, such as the dictionary a
    -- string literal is converted with, at the same position as the code they
    -- are inserted into, and those are about something else.
    isWritten :: ContextInfo -> Bool
    isWritten = \case
      Use -> True
      RecField _ _ -> True
      _ -> False

    nodesOf :: HieAST a -> [HieAST a]
    nodesOf ast = ast : concatMap nodesOf (nodeChildren ast)

    -- A line or a column is never negative, so this only changes the type.
    intToWord :: Int -> Word
    intToWord = toEnum . max 0
