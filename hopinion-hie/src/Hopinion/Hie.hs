-- | What the compiler wrote down about a module: the @.hie@ file for what it
-- names, the @.hi@ interface for what it declares.
--
-- Its own package because these are read by the @ghc@ library and hopinion
-- parses with @ghc-lib-parser@; both expose modules under @GHC.@, so one
-- module cannot import both without @PackageImports@.
-- hopinion itself only ever handles the two files as the opaque values here.
--
-- Each file is handed over whole, as the compiler's own type, so that a rule
-- wanting something nothing here reads can read it for itself: it depends on
-- @ghc@ and unwraps the value, and this package does not change.
-- What the shipped rules read out of them is a function over the whole file,
-- kept here so that every rule asks it the same way.
--
-- The version coupling that leaves is real and contained: the reader has to be
-- built by the compiler that built the code under check, which the Nix layer
-- arranges by taking both from one package set.
module Hopinion.Hie
  ( CompiledModule,
    compiledModuleHieFile,
    compiledModuleName,
    compiledModuleFile,
    compiledModuleGenerated,
    SourceRange (..),
    TypeConstructor (..),
    AppliedType (..),
    compiledModuleTypeAt,
    compiledModuleConstructorNamedAt,
    ArtifactUnreadable (..),
    renderArtifactUnreadable,
    readCompiledModule,
    CompiledInterface (..),
    readCompiledInterface,
    DeclaredInstance (..),
    compiledInterfaceInstances,
  )
where

import Control.Exception (SomeException, displayException, evaluate, fromException, tryJust)
import Data.Array (Array, bounds, (!))
import Data.Ix (inRange)
import qualified Data.Map as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Iface.Binary (CheckHiWay (..), TraceBinIFace (..), readBinIface)
import GHC.Iface.Ext.Binary (hie_file_result, readHieFile)
import GHC.Iface.Ext.Types
  ( HieAST (..),
    HieArgs (..),
    HieFile (..),
    HieType (..),
    HieTypeFlat,
    NodeInfo (..),
    TypeIndex,
    getAsts,
    getSourcedNodeInfo,
    hie_asts,
  )
import GHC.Iface.Syntax (IfaceClsInst (..))
import GHC.Iface.Type (IfaceTyCon (..))
import GHC.Platform (genericPlatform)
import GHC.Platform.Profile (Profile (..))
import GHC.Types.Name (Name, nameModule_maybe, nameOccName)
import GHC.Types.Name.Cache (initNameCache)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.SrcLoc (RealSrcSpan, srcSpanEndCol, srcSpanEndLine, srcSpanStartCol, srcSpanStartLine)
import GHC.Unit.Module (moduleName, moduleNameString)
import GHC.Unit.Module.ModIface (ModIface, mi_insts)
import Path (Abs, File, Path, Rel, parseRelFile, toFilePath)
import System.IO.Error (isDoesNotExistError)

-- | One module's @.hie@ file, as the compiler wrote it.
--
-- The constructor stays here, so that what is worked out of the file once per
-- read cannot disagree with the file.
data CompiledModule = CompiledModule
  { compiledModuleHieFile :: !HieFile,
    -- | What this module's generated code named: one set of names per splice
    -- expansion.
    -- Worked out once per read, since a rule asks it once per pair of names.
    --
    -- Generated code arrives as one node carrying the union of every name it
    -- produced, where ordinary code spreads its names across a leaf each. So a
    -- node naming two things is the only place two names can be known to have
    -- arrived together, which is what @genValidSpec \@T@ needs and a
    -- module-wide name set cannot give.
    --
    -- A @.hie@ file keeps no shape for generated code, so which class went
    -- with which type comes from the interface instead.
    compiledModuleGenerated :: ![Set Text]
  }

compiledModuleName :: CompiledModule -> Text
compiledModuleName = T.pack . moduleNameString . moduleName . hie_module . compiledModuleHieFile

-- | The source file GHC compiled, as GHC was given it: relative to the
-- package, not the repository. A module name does not identify a module, and
-- two @Main@s are told apart by this and nothing else.
--
-- Nothing when GHC recorded something that is not a package-relative file, an
-- absolute path included. Such a build says nothing that tells its own modules
-- apart, so whatever matches a module against one of these has nothing to
-- match on and must match none rather than guess.
compiledModuleFile :: CompiledModule -> Maybe (Path Rel File)
compiledModuleFile = parseRelFile . hie_hs_file . compiledModuleHieFile

generatedIn :: HieFile -> [Set Text]
generatedIn = concatMap generatedOf . M.elems . getAsts . hie_asts

-- | A range of the source a module was compiled from, counted the way GHC
-- counts: lines and columns from one, and the end column one past the last
-- character.
--
-- Its own type rather than a span from the parser, because this package sees
-- neither the parser nor hopinion.
-- A parse and a @.hie@ file of one module count the same way, so a range read
-- off one names the same code in the other.
data SourceRange = SourceRange
  { sourceRangeStartLine :: !Int,
    sourceRangeStartColumn :: !Int,
    sourceRangeEndLine :: !Int,
    sourceRangeEndColumn :: !Int
  }
  deriving (Eq)

-- | A type constructor, told apart from another of the same name by the module
-- that defines it.
data TypeConstructor = TypeConstructor
  { typeConstructorModule :: !Text,
    typeConstructorName :: !Text
  }
  deriving (Show, Eq)

-- | The constructor at the head of a type, with the constructor at the head of
-- each argument it is applied to.
--
-- The arguments are what tell one instance of a data family from another, since
-- every instance has the family at its head.
-- An argument whose head is not a constructor, such as a type variable, is
-- 'Nothing'.
data AppliedType = AppliedType
  { appliedTypeConstructor :: !TypeConstructor,
    appliedTypeArguments :: ![Maybe TypeConstructor]
  }
  deriving (Show, Eq)

-- | The head of the type the compiler gave the code written at exactly this
-- range, such as a pattern.
--
-- Nothing when the compiler gave that code no type, or gave it one whose head
-- is not a constructor, such as a type variable or a function type.
-- A synonym is not seen through.
-- A @.hie@ file keeps one copy of each type, written the way some code in the
-- module first wrote it, so a type can come back written as one of its synonyms
-- even where the code at this range never names that synonym.
-- 'compiledModuleConstructorNamedAt' is what resolves a synonym.
compiledModuleTypeAt :: CompiledModule -> SourceRange -> Maybe AppliedType
compiledModuleTypeAt compiled range =
  let file = compiledModuleHieFile compiled
   in listToMaybe
        [ th
        | info <- nodeInfosAt range file,
          ty <- nodeType info,
          Just th <- [appliedTypeOf (hie_types file) ty]
        ]

-- | The type constructor that the code written at exactly this range names,
-- such as the head of the right-hand side of a type synonym.
--
-- Nothing when that code names no type constructor, such as a type variable.
compiledModuleConstructorNamedAt :: CompiledModule -> SourceRange -> Maybe TypeConstructor
compiledModuleConstructorNamedAt compiled range =
  listToMaybe
    [ tc
    | info <- nodeInfosAt range (compiledModuleHieFile compiled),
      Right name <- M.keys (nodeIdentifiers info),
      Just tc <- [typeConstructorOfName name]
    ]

nodeInfosAt :: SourceRange -> HieFile -> [NodeInfo TypeIndex]
nodeInfosAt range file =
  [ info
  | ast <- M.elems (getAsts (hie_asts file)),
    node <- nodesAt range ast,
    info <- M.elems (getSourcedNodeInfo (sourcedNodeInfo node))
  ]

-- | Every node at exactly this range, descending only into the nodes that
-- contain it, since a node's children lie within it.
nodesAt :: SourceRange -> HieAST a -> [HieAST a]
nodesAt range ast =
  let contains :: SourceRange -> Bool
      contains outer =
        (sourceRangeStartLine outer, sourceRangeStartColumn outer) <= (sourceRangeStartLine range, sourceRangeStartColumn range)
          && (sourceRangeEndLine range, sourceRangeEndColumn range) <= (sourceRangeEndLine outer, sourceRangeEndColumn outer)
   in [ast | rangeOf (nodeSpan ast) == range]
        ++ concatMap (nodesAt range) [c | c <- nodeChildren ast, contains (rangeOf (nodeSpan c))]

rangeOf :: RealSrcSpan -> SourceRange
rangeOf sp =
  SourceRange
    { sourceRangeStartLine = srcSpanStartLine sp,
      sourceRangeStartColumn = srcSpanStartCol sp,
      sourceRangeEndLine = srcSpanEndLine sp,
      sourceRangeEndColumn = srcSpanEndCol sp
    }

-- | The head of one of the file's types, with the heads of the arguments a
-- reader of the code would write, which leaves out the invisible ones such as
-- kinds.
--
-- An index out of the file's own table is not one the compiler writes, and is
-- read as a type with no constructor rather than trusted.
appliedTypeOf :: Array TypeIndex HieTypeFlat -> TypeIndex -> Maybe AppliedType
appliedTypeOf types i
  | not (inRange (bounds types) i) = Nothing
  | otherwise = case types ! i of
      HTyConApp tc (HieArgs args) -> do
        constructor <- typeConstructorOfName (ifaceTyConName tc)
        pure
          AppliedType
            { appliedTypeConstructor = constructor,
              appliedTypeArguments = [appliedTypeConstructor <$> appliedTypeOf types a | (True, a) <- args]
            }
      HTyVarTy _ -> Nothing
      HAppTy _ _ -> Nothing
      HForAllTy _ _ -> Nothing
      HFunTy {} -> Nothing
      HQualTy _ _ -> Nothing
      HLitTy _ -> Nothing
      HCastTy _ -> Nothing
      HCoercionTy -> Nothing

typeConstructorOfName :: Name -> Maybe TypeConstructor
typeConstructorOfName name = do
  defining <- nameModule_maybe name
  pure
    TypeConstructor
      { typeConstructorModule = T.pack (moduleNameString (moduleName defining)),
        typeConstructorName = T.pack (occNameString (nameOccName name))
      }

-- | Why a file the compiler was supposed to have written could not be read.
--
-- One constructor, because there is one answer to give: the reader threw, and
-- what it threw is GHC's own words about a binary format this build does not
-- speak. Named rather than left as a bare 'Text' so that a caller has to
-- render it before showing it to anybody.
newtype ArtifactUnreadable = ArtifactUnreadable Text

renderArtifactUnreadable :: ArtifactUnreadable -> Text
renderArtifactUnreadable (ArtifactUnreadable t) = t

-- | Read one @.hie@ file, or say why it could not be read.
--
-- A file written by another compiler is the expected failure, reported rather
-- than thrown so the caller can decide what knowing nothing means.
-- A file that is not there is thrown, so that the caller tells absence from
-- unreadability the way it would for any other file.
readCompiledModule :: Path Abs File -> IO (Either ArtifactUnreadable CompiledModule)
readCompiledModule path = do
  result <- tryJust unlessAbsent $ do
    cache <- initNameCache 'z' []
    file <- hie_file_result <$> readHieFile cache (toFilePath path)
    let compiled =
          CompiledModule
            { compiledModuleHieFile = file,
              compiledModuleGenerated = generatedIn file
            }
    -- Forced inside the catch, so a file this compiler cannot read fails here
    -- rather than as an exception in the middle of a rule.
    _ <- evaluate (T.length (compiledModuleName compiled))
    _ <- evaluate (length (compiledModuleGenerated compiled))
    pure compiled
  pure $ case result of
    Left e -> Left (reason e)
    Right compiled -> Right compiled

-- | Every exception but the one that says the file is not there.
unlessAbsent :: SomeException -> Maybe SomeException
unlessAbsent e = case fromException e of
  Just io | isDoesNotExistError io -> Nothing
  _ -> Just e

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

-- | One module's @.hi@ interface, as the compiler wrote it.
newtype CompiledInterface = CompiledInterface
  { unCompiledInterface :: ModIface
  }

-- | Read one @.hi@ interface, or say why it could not be read, and throw for a
-- file that is not there, as 'readCompiledModule' does.
--
-- A generic platform, no ways and an ignored way tag, because nothing read out
-- of an interface here depends on the flavour of the build.
readCompiledInterface :: Path Abs File -> IO (Either ArtifactUnreadable CompiledInterface)
readCompiledInterface path = do
  result <- tryJust unlessAbsent $ do
    cache <- initNameCache 'z' []
    iface <- CompiledInterface <$> readBinIface profile cache IgnoreHiWay QuietBinIFace (toFilePath path)
    -- Forced inside the catch, for the reason the .hie read is.
    _ <- evaluate (length (compiledInterfaceInstances iface))
    pure iface
  pure $ case result of
    Left e -> Left (reason e)
    Right iface -> Right iface
  where
    profile =
      Profile
        { profilePlatform = genericPlatform,
          profileWays = mempty
        }

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
    -- | The module that defines the class, which tells two classes of one name
    -- apart.
    declaredInstanceClassModule :: !Text,
    declaredInstanceType :: !Text
  }
  deriving (Show, Eq)

-- | Every instance one module declares whose head is a type constructor.
compiledInterfaceInstances :: CompiledInterface -> [DeclaredInstance]
compiledInterfaceInstances = mapMaybe declaredInstanceOf . mi_insts . unCompiledInterface

-- | An instance whose head is not a type constructor is not one an obligation
-- can be about, since there is no type to write @genValidSpec \@T@ at.
--
-- The last head type rather than the first: an interface records the invisible
-- arguments too, so a poly-kinded class has its kind in front and arrives as
-- @[TYPE, Widget]@ where an ordinary one is @[Written]@. Measured, not read
-- from the manual.
--
-- A class is a top-level name, so it always has a module, and an instance
-- whose class has none is not one an interface can record.
declaredInstanceOf :: IfaceClsInst -> Maybe DeclaredInstance
declaredInstanceOf inst = case (reverse (ifInstTys inst), nameModule_maybe (ifInstCls inst)) of
  (Just tycon : _, Just classModule) ->
    Just
      DeclaredInstance
        { declaredInstanceClass = T.pack (occNameString (nameOccName (ifInstCls inst))),
          declaredInstanceClassModule = T.pack (moduleNameString (moduleName classModule)),
          declaredInstanceType = T.pack (occNameString (nameOccName (ifaceTyConName tycon)))
        }
  _ -> Nothing
