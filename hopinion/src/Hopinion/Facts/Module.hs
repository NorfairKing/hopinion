{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hopinion.Facts.Module
  ( ModuleContext (..),
    moduleContextRef,
  )
where

import GHC.Generics (Generic)
import Hopinion.Check.Hs.NoSemigroupOnText.Fact
import Hopinion.Comment (CommentFact (..))
import Hopinion.Facts.Component
import Hopinion.Facts.Instance
import Hopinion.Facts.Name
import Hopinion.Facts.Occurrence
import Hopinion.Facts.Outcome
import Hopinion.Facts.Place
import Hopinion.Facts.Suppression
import Hopinion.Facts.TemplateHaskell
import Hopinion.Facts.TypeApp
import Path (File, Path, Rel)

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
    moduleContextInstances :: ![InstanceFact],
    moduleContextNames :: ![NameFact],
    moduleContextComments :: ![CommentFact],
    moduleContextAnnotations :: ![AnnotationFact],
    moduleContextAnnotationProblems :: ![AnnotationProblem],
    moduleContextTypeApps :: ![TypeAppFact],
    moduleContextConcatChains :: ![ConcatChain],
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
