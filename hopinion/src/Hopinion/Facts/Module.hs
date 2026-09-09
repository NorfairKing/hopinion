{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Facts.Module
  ( ModuleContext (..),
    moduleContextRef,
    moduleContextIsSpecFile,
    isSpecFilePath,
  )
where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Hopinion.Check.Hs.LambdaCase.Fact
import Hopinion.Check.Hs.NoSemigroupOnText.Fact
import Hopinion.Check.Package.AppOnlyMain.Fact
import Hopinion.Comment (CommentFact (..))
import Hopinion.Facts.Component
import Hopinion.Facts.Decl
import Hopinion.Facts.Export
import Hopinion.Facts.Instance
import Hopinion.Facts.Name
import Hopinion.Facts.Occurrence
import Hopinion.Facts.Outcome
import Hopinion.Facts.Place
import Hopinion.Facts.Suppression
import Hopinion.Facts.TemplateHaskell
import Hopinion.Facts.TypeApp
import Path (File, Path, Rel, filename)

-- | Everything the parser saw about one module.
--
-- Never serialised, and that is the point: a rule that reads one runs in the
-- process that produced it, so a fact it turns straight into a finding is never
-- written down. What a rule does have to carry, that rule says.
data ModuleContext = ModuleContext
  { moduleContextModule :: !ModuleKey,
    moduleContextPath :: !(Path Rel File),
    moduleContextComponent :: !ComponentKind,
    -- | Which component, by name, so that two modules GHC both calls Main are
    -- two modules here as well.
    moduleContextComponentName :: !ComponentName,
    -- | Every top-level declaration, in source order.
    moduleContextDecls :: ![DeclFact],
    moduleContextExports :: !ExportList,
    moduleContextInstances :: ![InstanceFact],
    moduleContextNames :: ![NameFact],
    moduleContextComments :: ![CommentFact],
    moduleContextAnnotations :: ![AnnotationFact],
    moduleContextAnnotationProblems :: ![AnnotationProblem],
    moduleContextTypeApps :: ![TypeAppFact],
    moduleContextConcatChains :: ![ConcatChain],
    moduleContextCasedArguments :: ![CasedArgument],
    moduleContextStrayAppDecls :: ![StrayAppDecl],
    moduleContextTemplateHaskell :: !TemplateHaskellUse,
    moduleContextOutcome :: !ParseOutcome
  }
  deriving stock (Show, Eq, Generic)

-- | Which module this is, in which component, which is what a scope names.
moduleContextRef :: ModuleContext -> ModuleRef
moduleContextRef ctx =
  ModuleRef
    { moduleRefComponent = moduleContextComponentName ctx,
      moduleRefModule = moduleContextModule ctx
    }

-- | Whether this module is a test file, which is what the rules about test
-- files are asking about.
--
-- The file name is the whole of it, and it is the convention sydtest and every
-- discovery mechanism over it already run on: the tests for a module are in a
-- file named after that module with @Spec@ on the end. Read here rather than in
-- each rule, so that what a test file is has one answer.
moduleContextIsSpecFile :: ModuleContext -> Bool
moduleContextIsSpecFile = isSpecFilePath . moduleContextPath

-- | The file name alone, so that what a test file is can be asked of a path.
--
-- A file named exactly @Spec.hs@ is not one, even though @Spec@ is on the end
-- of it. That is the entry point discovery generates from, and a preprocessor
-- writes its module header, so a rule reading the source for an export list or
-- for what the file binds is reading a file that has neither and cannot be
-- given either.
isSpecFilePath :: Path Rel File -> Bool
isSpecFilePath path =
  let name = relPathText (filename path)
   in T.isSuffixOf "Spec.hs" name && name /= "Spec.hs"
