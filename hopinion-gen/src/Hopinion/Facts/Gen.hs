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
import Hopinion.Choices (Choices)
import Hopinion.Comment (Attachment, CommentFact, CommentStyle, RawComment)
import Hopinion.Facts
import Hopinion.Report (Complaint, Complaints, Failure, ParseFailure, StoreProblem)
import Hopinion.Rule (Finding)
import Hopinion.Rule.Id
import Test.QuickCheck (elements)

instance GenValid RawComment

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

instance GenValid Level

instance GenValid ModuleKey

instance GenValid DeclName

instance GenValid PackageName

instance GenValid TypeHead

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

instance GenValid Span

instance GenValid ComponentName

instance GenValid ModuleRef

instance GenValid ScopeKey

instance GenValid ComponentKind

instance GenValid ParseOutcome

instance GenValid DeclKind

instance GenValid DeclFact

instance GenValid InstanceMethods

instance GenValid InstanceOrigin

instance GenValid InstanceFact

instance GenValid Finding

instance GenValid ParseFailure

instance GenValid StoreProblem

instance GenValid Failure

instance GenValid Complaint

instance GenValid Complaints

instance GenValid CommentStyle

instance GenValid Attachment

instance GenValid CommentFact

instance GenValid AnnotationPrecision

instance GenValid Reason

instance GenValid AnnotationFact

instance GenValid AnnotationProblem

instance GenValid TypeAppFact

instance GenValid ConcatOperand

instance GenValid ConcatChain

instance GenValid TemplateHaskellUse

instance GenValid PackageRole

instance GenValid FormatVersion

instance GenValid GenPackage

instance GenValid Unused

instance GenValid OverBroad

instance GenValid Choices
