{-# LANGUAGE DeriveGeneric #-}

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
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)
import GHC.Iface.Binary (CheckHiWay (..), TraceBinIFace (..), readBinIface)
import GHC.Iface.Ext.Binary (hie_file_result, readHieFile)
import GHC.Iface.Ext.Types
  ( HieAST (..),
    HieFile (..),
    NodeInfo (..),
    getAsts,
    getSourcedNodeInfo,
    hie_asts,
  )
import GHC.Iface.Syntax (IfaceClsInst (..))
import GHC.Iface.Type (IfaceTyCon (..))
import GHC.Platform (genericPlatform)
import GHC.Platform.Profile (Profile (..))
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Cache (initNameCache)
import GHC.Types.Name.Occurrence (occNameString)
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
    declaredInstanceType :: !Text
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity DeclaredInstance

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
declaredInstanceOf :: IfaceClsInst -> Maybe DeclaredInstance
declaredInstanceOf inst = case reverse (ifInstTys inst) of
  (Just tycon : _) ->
    Just
      DeclaredInstance
        { declaredInstanceClass = T.pack (occNameString (nameOccName (ifInstCls inst))),
          declaredInstanceType = T.pack (occNameString (nameOccName (ifaceTyConName tycon)))
        }
  _ -> Nothing
