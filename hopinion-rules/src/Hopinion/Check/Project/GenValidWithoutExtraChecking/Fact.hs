{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Project.GenValidWithoutExtraChecking.Fact
  ( GenValidMethod (..),
    genValidMethods,
    genValidMethodName,
    genValidMethodReplacement,
    WrittenInstance (..),
    DefaultingGenValid (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Hopinion.Facts.Instance
import Hopinion.Facts.Place

-- | A method of GenValid whose default checks the validity of what it makes.
data GenValidMethod
  = GenValidMethodGenValid
  | GenValidMethodShrinkValid

genValidMethods :: [GenValidMethod]
genValidMethods = [GenValidMethodGenValid, GenValidMethodShrinkValid]

genValidMethodName :: GenValidMethod -> Text
genValidMethodName = \case
  GenValidMethodGenValid -> "genValid"
  GenValidMethodShrinkValid -> "shrinkValid"

-- | What the method is defined as instead when nothing but the fields decides
-- validity.
genValidMethodReplacement :: GenValidMethod -> Text
genValidMethodReplacement = \case
  GenValidMethodGenValid -> "genValidStructurallyWithoutExtraChecking"
  GenValidMethodShrinkValid -> "shrinkValidStructurallyWithoutExtraFiltering"

-- | An instance whose methods the source shows, with where it names its class
-- and its type.
--
-- Where rather than what, because the source names both the way they were
-- imported: two types called @Score@ from two modules look the same here, and
-- only the compiler can say which one each occurrence is.
data WrittenInstance = WrittenInstance
  { writtenInstanceFact :: !InstanceFact,
    writtenInstanceClassAt :: !Span,
    writtenInstanceTypeAt :: !Span,
    writtenInstanceMethods :: ![Text]
  }

-- | A GenValid instance that leaves at least one method to its default.
--
-- Whether that default checks anything a structural generator could get wrong
-- depends on the Validity instance of the same type, which is usually in
-- another package.
data DefaultingGenValid = DefaultingGenValid
  { defaultingGenValidInstance :: !WrittenInstance,
    defaultingGenValidMethods :: !(NonEmpty GenValidMethod)
  }
