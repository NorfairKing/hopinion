{-# LANGUAGE OverloadedStrings #-}

-- | Finding what a compiler wrote down about the modules under check.
--
-- The reading itself is in @hopinion-hie@, which is version-locked to a
-- compiler and therefore kept at arm's length. This is the part that decides
-- which file belongs to which module, what a missing one means, and when to go
-- and look.
module Hopinion.Compiled
  ( HieDirectories (..),
    noHieDirectories,
    CompiledModules,
    ArtifactProblem (..),
    compiledModulesFor,
    uncoveredModules,
    compiledModuleOf,
    declaredInstancesOf,
    couldGenerateUseOf,
    sameSourceFile,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (filterM)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Hopinion.Facts
import Hopinion.Hie (readCompiledModule, readDeclaredInstances, renderArtifactUnreadable)
import Path (Abs, Dir, File, Path, Rel, parseRelFile, toFilePath, (</>))
import Path.IO (forgivingAbsence, getModificationTime)
import qualified System.FilePath as FP
import UnliftIO.Memoize (Memoized, memoizeMVar, runMemoized)

-- | Where a build collected its @.hie@ output.
--
-- Empty means no build was involved, and every question a rule would have asked
-- goes unanswered. Non-empty is a promise that these directories account for
-- every module read, which 'uncoveredModules' holds the run to.
newtype HieDirectories = HieDirectories [Path Abs Dir]
  deriving (Show, Eq)

instance Semigroup HieDirectories where
  HieDirectories a <> HieDirectories b = HieDirectories (nub (a ++ b))

instance Monoid HieDirectories where
  mempty = noHieDirectories

noHieDirectories :: HieDirectories
noHieDirectories = HieDirectories []

-- | What a build has to say about the modules of one scope, none of it read.
--
-- Memoised rather than read, because reading a @.hie@ file costs more than
-- parsing the module did and only the rules know which are worth reading.
-- @unliftio@'s, which runs the read at most once however many rules ask.
--
-- Keyed by file, because a module name is not an identity: a package with four
-- test suites has four modules called @Main@.
data CompiledModules = CompiledModules
  { compiledModulesDirs :: !HieDirectories,
    compiledModulesByFile :: !(Map (Path Rel File) ModuleArtifacts)
  }

-- | What a build left about one module, and what it takes to go and get it.
--
-- Two files, read separately, because a module asked one is rarely asked the
-- other: what it names is in its @.hie@ file, what it declares is in its
-- interface.
data ModuleArtifacts = ModuleArtifacts
  { moduleArtifactsKey :: !ModuleKey,
    moduleArtifactsCompiled :: !(Memoized (Answer CompiledModule)),
    moduleArtifactsDeclared :: !(Memoized (Answer [DeclaredInstance]))
  }

-- | Three answers, not two: a file that is not there and a file that is there
-- and cannot be used have different fixes, and reporting the second as the
-- first sends a reader looking for a file they are standing on.
data Answer a
  = Answered !a
  | -- | Nothing at any of the paths a build would have written to.
    NothingThere
  | -- | Something at one of them, and why it did not answer: written by a
    -- different compiler, or about a different module.
    Unusable ![Text]

-- | A build was given and could not answer for a module it promised to cover.
--
-- Its own type so the command layer can catch exactly this and make it a
-- complaint: a producing command that dies has written no report, which is what
-- the producing-and-judging split exists to prevent.
newtype ArtifactProblem = ArtifactProblem Text
  deriving (Show, Eq)

instance Exception ArtifactProblem

-- | A project scope is every package's. Combining shares what is deferred
-- rather than deferring again, so one module stays one read.
instance Semigroup CompiledModules where
  a <> b =
    CompiledModules
      { compiledModulesDirs = compiledModulesDirs a <> compiledModulesDirs b,
        compiledModulesByFile = M.union (compiledModulesByFile a) (compiledModulesByFile b)
      }

instance Monoid CompiledModules where
  mempty = CompiledModules {compiledModulesDirs = mempty, compiledModulesByFile = M.empty}

-- | Every one of these modules, ready to be asked about and not yet looked for.
--
-- Named by key and by file: the key alone does not identify a module, and the
-- file alone does not say where the compiler put its answer.
--
-- Only modules read as Haskell belong here. A preprocessor's output has no
-- source this tool saw, so a build is not held to answering for it.
compiledModulesFor :: HieDirectories -> [(ModuleKey, Path Rel File)] -> IO CompiledModules
compiledModulesFor dirs mks = do
  entries <- traverse artifactsFor mks
  pure
    CompiledModules
      { compiledModulesDirs = dirs,
        compiledModulesByFile = M.fromList entries
      }
  where
    artifactsFor (mk, rp) = do
      compiled <- memoizeMVar (lookUp dirs mk rp)
      declared <- memoizeMVar (lookUpInterface dirs mk rp)
      pure
        ( rp,
          ModuleArtifacts
            { moduleArtifactsKey = mk,
              moduleArtifactsCompiled = compiled,
              moduleArtifactsDeclared = declared
            }
        )

-- | Every module the build promised to account for and did not.
--
-- An artifact tree missing a module is indistinguishable at the point of use
-- from no artifacts at all, so a build that covers less than it was read from
-- is a failure rather than a quieter check.
--
-- Both files, since a module missing either is a module some question cannot be
-- answered about. Asked of the file system rather than of the files, so this
-- stays a stat per module and the reading stays deferred.
uncoveredModules :: CompiledModules -> IO [Path Rel File]
uncoveredModules cm = case compiledModulesDirs cm of
  HieDirectories [] -> pure []
  dirs ->
    map fst
      <$> filterM
        (\(_, artifacts) -> not <$> covered dirs (moduleArtifactsKey artifacts))
        (M.toList (compiledModulesByFile cm))
  where
    covered dirs mk = do
      names <- anyDirectoryHolds dirs (artifactFileOf "hie" mk)
      declarations <- anyDirectoryHolds dirs (artifactFileOf "hi" mk)
      pure (names && declarations)

-- | Whether a build wrote this file anywhere it was told to look.
--
-- A stat rather than a read, which is what keeps 'uncoveredModules' cheap, and
-- 'forgivingAbsence' rather than @doesFileExist@ so that absence arrives the way
-- every other absence in this tool does.
anyDirectoryHolds :: HieDirectories -> Maybe (Path Rel File) -> IO Bool
anyDirectoryHolds _ Nothing = pure False
anyDirectoryHolds (HieDirectories dirs) (Just relative) =
  any isJust <$> traverse (\d -> forgivingAbsence (getModificationTime (d </> relative))) dirs

-- | Where a module's file is, by the layout the compiler writes: @A.B.C@ is
-- @A\/B\/C.hie@ or @A\/B\/C.hi@ under one of the directories.
artifactFileOf :: String -> ModuleKey -> Maybe (Path Rel File)
artifactFileOf extension mk =
  parseRelFile (FP.joinPath (map T.unpack (T.splitOn "." (moduleKeyText mk))) FP.<.> extension)

compiledModuleOf :: Path Rel File -> CompiledModules -> IO (Maybe CompiledModule)
compiledModuleOf rp cm = answerFor rp cm moduleArtifactsCompiled

-- | Every instance this module declares, which is what makes generated code
-- answerable.
--
-- A @.hie@ file says which names an expansion mentioned, never which class went
-- with which type. An interface says what the module ended up declaring,
-- generated or not, which is the question an obligation's made side asks.
declaredInstancesOf :: Path Rel File -> CompiledModules -> IO (Maybe [DeclaredInstance])
declaredInstancesOf rp cm = answerFor rp cm moduleArtifactsDeclared

-- | Asking, which is what reads the file, the first time anything asks.
--
-- A module of another scope is not here at all: nothing known and nothing
-- promised. A module that is here and whose file cannot be found is the other
-- thing entirely, since a build said it would be.
answerFor :: Path Rel File -> CompiledModules -> (ModuleArtifacts -> Memoized (Answer a)) -> IO (Maybe a)
answerFor rp cm which = case M.lookup rp (compiledModulesByFile cm) of
  Nothing -> pure Nothing
  Just artifacts -> do
    found <- runMemoized (which artifacts)
    case (found, compiledModulesDirs cm) of
      (Answered answer, _) -> pure (Just answer)
      (_, HieDirectories []) -> pure Nothing
      (NothingThere, HieDirectories dirs) ->
        promisedButAbsent (moduleArtifactsKey artifacts) rp [concat ["nothing at ", toFilePath d] | d <- dirs]
      (Unusable whys, HieDirectories _) ->
        promisedButAbsent (moduleArtifactsKey artifacts) rp (map T.unpack whys)

-- | The build promised this module and did not answer for it, said with what
-- was found at each place it should have been.
--
-- Thrown rather than returned: the caller is a rule, and a rule has no third
-- answer to give. The command layer catches it and makes it a complaint.
promisedButAbsent :: ModuleKey -> Path Rel File -> [String] -> IO a
promisedButAbsent mk rp whys =
  throwIO
    ( ArtifactProblem
        ( T.pack
            ( unlines
                ( unwords
                    [ "A build's .hie files were given, so every module has to be covered by them, but",
                      T.unpack (moduleKeyText mk),
                      "from",
                      T.unpack (relPathText rp),
                      "is not. A rule asked what that module's code generates and would have been",
                      "told nothing, which reads exactly like a module that generates nothing."
                    ]
                    : map ("  " ++) whys
                )
            )
        )
    )

-- | Whether code this module generates could be a use of one name at another,
-- which is what a call with a type application is.
--
-- Two names in a module are not two names in one place: a test that mentions
-- @T@ and another that calls @genValidSpec@ say nothing about
-- @genValidSpec \@T@. So this asks whether any single expansion names both, and
-- a module no build spoke for could have generated anything.
couldGenerateUseOf :: Text -> Text -> Path Rel File -> CompiledModules -> IO Bool
couldGenerateUseOf caller callee rp compiled = do
  found <- compiledModuleOf rp compiled
  pure $ case found of
    Nothing -> True
    Just c -> any (\names -> S.member caller names && S.member callee names) (compiledModuleGenerated c)

-- | Whether a @.hie@ file's source is the file a module was read from.
--
-- By trailing path components, because the two are relative to different
-- things: GHC records the path cabal handed it, relative to the package, and a
-- module here is named relative to the repository.
--
-- Two components at least. @Main.hs@ ends the path of every component with a
-- @main-is@, so a bare file name would let one test suite's compiled module be
-- read as another's, and an empty path agrees with everything. A build handing
-- the compiler absolute paths therefore matches nothing, which is safe.
sameSourceFile :: Maybe (Path Rel File) -> Path Rel File -> Bool
sameSourceFile compiled rp = case compiled of
  Nothing -> False
  Just file -> case (backwards (toFilePath file), backwards (T.unpack (relPathText rp))) of
    (theirs@(_ : _ : _), ours@(_ : _ : _)) -> and (zipWith (==) theirs ours)
    _ -> False
  where
    backwards = reverse . FP.splitDirectories

-- | The first directory holding this module's file, where holding it means the
-- file is about this module and about the source it was read from.
--
-- Both, because neither is enough: another package's tree may hold a file at
-- the same path, and a @Main.hie@ belongs to whichever component wrote it.
--
-- Why a file that is there did not answer is kept rather than discarded: a
-- @.hie@ file this build cannot parse is the expected failure, and the reason
-- is what says which compiler wrote it.
lookUp :: HieDirectories -> ModuleKey -> Path Rel File -> IO (Answer CompiledModule)
lookUp (HieDirectories dirs) mk rp = case artifactFileOf "hie" mk of
  Nothing -> pure NothingThere
  Just relative -> go relative [] dirs
  where
    go :: Path Rel File -> [Text] -> [Path Abs Dir] -> IO (Answer CompiledModule)
    go _ whys [] = pure (if null whys then NothingThere else Unusable (reverse whys))
    -- Read rather than asked about and then read: a file that is there for the
    -- question and gone for the read would be an exception out of a rule.
    go relative whys (d : rest) = do
      let file = d </> relative
      let path = toFilePath file
      result <- forgivingAbsence (readCompiledModule file)
      case result of
        Nothing -> go relative whys rest
        Just read' ->
          case read' of
            Left err -> go relative (unreadable path (renderArtifactUnreadable err) : whys) rest
            Right compiled
              | compiledModuleName compiled /= moduleKeyText mk ->
                  go relative (aboutAnother path (compiledModuleName compiled) : whys) rest
              | not (sameSourceFile (compiledModuleFile compiled) rp) ->
                  go relative (fromAnother path (compiledModuleFile compiled) : whys) rest
              | otherwise -> pure (Answered compiled)

    unreadable path err =
      T.pack (unwords [path, "is there and this build of hopinion cannot read it:", T.unpack err])

    aboutAnother path name =
      T.pack (unwords [path, "is there and is about", T.unpack name, "rather than", T.unpack (moduleKeyText mk)])

    fromAnother path file =
      T.pack
        ( unwords
            [ path,
              "is there and was compiled from",
              maybe "a source file that is not one this tool could name" toFilePath file,
              "rather than",
              T.unpack (relPathText rp)
            ]
        )

-- | The interface beside the @.hie@ file, in the same tree, for the same module.
--
-- Which tree is decided by the @.hie@ file rather than by the interface,
-- because an interface carries no source path and so cannot say which of two
-- components' modules called @Main@ it belongs to. The @.hie@ file can, so it
-- picks the directory and the interface is read from the one it picked.
lookUpInterface :: HieDirectories -> ModuleKey -> Path Rel File -> IO (Answer [DeclaredInstance])
lookUpInterface (HieDirectories ds) mk rp = case artifactFileOf "hi" mk of
  Nothing -> pure NothingThere
  Just relative -> go relative [] ds
  where
    go :: Path Rel File -> [Text] -> [Path Abs Dir] -> IO (Answer [DeclaredInstance])
    go _ whys [] = pure (if null whys then NothingThere else Unusable (reverse whys))
    go relative whys (d : rest) = do
      here <- lookUp (HieDirectories [d]) mk rp
      case here of
        NothingThere -> go relative whys rest
        Unusable theirs -> go relative (reverse theirs ++ whys) rest
        Answered _ -> do
          let file = d </> relative
          let path = toFilePath file
          result <- forgivingAbsence (readDeclaredInstances file)
          case result of
            -- Kept, because this directory is the one that does hold this
            -- module: saying nothing here would report the module as nowhere,
            -- when its interface was not written beside its names.
            Nothing ->
              go relative (T.pack (unwords [path, "is not there, though the .hie beside it is about this module"]) : whys) rest
            Just (Left err) ->
              go
                relative
                (T.pack (unwords [path, "is there and this build of hopinion cannot read it:", T.unpack (renderArtifactUnreadable err)]) : whys)
                rest
            Just (Right declared) -> pure (Answered declared)
