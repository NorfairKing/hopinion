{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Generators for the fact types, in the gen package so they are not compiled
-- into the executable.
module Hopinion.Facts.Gen () where

import Data.GenValidity
import Data.GenValidity.Aeson ()
import Data.GenValidity.Containers ()
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import qualified Data.Text as T
import Hopinion.Annotation (OverBroad, Unused)
import Hopinion.Check.Hs.LambdaCase.Fact
import Hopinion.Check.Hs.NoFilePath.Fact
import Hopinion.Check.Hs.NoSemigroupOnText.Fact
import Hopinion.Check.Hs.NoStringError.Fact
import Hopinion.Check.Hs.TestOneSpecPerFile.Fact
import Hopinion.Check.Obligation.Fact
import Hopinion.Check.Package.AppOnlyMain.Fact
import Hopinion.Check.ProseError.Fact
import Hopinion.Choices (Choices)
import Hopinion.Comment (Attachment, CommentFact, CommentStyle, RawComment)
import Hopinion.Facts.Component
import Hopinion.Facts.Decl
import Hopinion.Facts.Instance
import Hopinion.Facts.Name
import Hopinion.Facts.Outcome
import Hopinion.Facts.Package
import Hopinion.Facts.Place
import Hopinion.Facts.Suppression
import Hopinion.Facts.Version
import Hopinion.Report (Complaint, Complaints, Failure, ParseFailure, StoreProblem)
import Hopinion.Rule (Finding)
import Hopinion.Rule.Id
import Test.QuickCheck (elements)

instance GenValid RawComment where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

-- | Built rather than filtered. An id is PascalCase and alphanumeric, which is
-- narrow enough that rejection sampling over arbitrary text finds one about
-- never: with the structural generator this spec took a minute and failed.
--
-- Shrinking truncates, which keeps the first character and so keeps the shape,
-- so every step of a shrink is still an id.
instance GenValid RuleId where
  genValid = do
    initial <- elements ['A' .. 'Z']
    rest <- genListOf (elements (concat [['A' .. 'Z'], ['a' .. 'z'], ['0' .. '9'], "_"]))
    pure (RuleId (T.pack (initial : rest)))
  shrinkValid (RuleId t) = [RuleId (T.take n t) | n <- [1 .. T.length t - 1]]

instance GenValid Level where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ModuleKey where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid DeclName where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid PackageName where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TypeHead where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid NonEmptyText where
  genValid = do
    c <- genValid
    t <- genValid
    case nonEmptyText (T.cons c t) of
      Just net -> pure net
      -- Unreachable: a text with a character consed on is never empty. Written
      -- as a retry rather than an error so the generator stays total.
      Nothing -> genValid
  shrinkValid _ = []

instance GenValid Position

instance GenValid Span where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ComponentName

instance GenValid ModuleRef where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ScopeKey where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ComponentKind where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ParseOutcome where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid DeclKind where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid DeclFact where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ExportList where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid InstanceMethods where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid InstanceOrigin where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid InstanceFact where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Finding where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ParseFailure where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid StoreProblem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Failure where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Complaint where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Complaints where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid CommentStyle where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Attachment where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid CommentFact where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid AnnotationPrecision where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Reason where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid AnnotationFact where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid AnnotationProblem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TypeAppFact where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ArgumentShape where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid CasedArgument where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid StrayAppDecl where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid NameFact where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ConcatOperand where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ConcatChain where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ErrorType where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ErrorSlot where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ErrorDeclShape where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ErrorDecl where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TemplateHaskellUse where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid PackageRole where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid FormatVersion where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid GenPackage where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Unused where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid OverBroad where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Choices where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
