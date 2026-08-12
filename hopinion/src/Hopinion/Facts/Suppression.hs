{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What a suppression is, once it has been read out of a comment.
--
-- Separate from the rest of the facts because these are the only ones nothing
-- in a repository produces: every other fact is something a module contains,
-- and these are what somebody wrote about a finding.
--
-- 'NonEmptyText' lives here rather than beside the other small types because
-- the mandatory reason is the only thing that wants it, and keeping the two in
-- one module is what keeps its constructor private. That privacy is the type:
-- 'nonEmptyText' is the only way to make one, so the empty reason is
-- unrepresentable rather than rejected at run time.
module Hopinion.Facts.Suppression
  ( NonEmptyText,
    nonEmptyText,
    nonEmptyTextText,
    AnnotationPrecision (..),
    Reason (..),
    adoptionReason,
    adoptionReasonText,
    AnnotationFact (..),
    AnnotationProblem (..),
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Validity
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Hopinion.Facts.Persist
import Hopinion.Facts.Place
import Hopinion.Rule.Id

-- | A suppression with no reason is a config exception with extra steps, so the
-- mandatory reason is unrepresentable to violate rather than checked at runtime.
--
-- Abstract on purpose: 'nonEmptyText' is the only way to make one, so a caller
-- cannot write the empty one by hand.
newtype NonEmptyText = NonEmptyText Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec NonEmptyText)

instance Validity NonEmptyText where
  validate net@(NonEmptyText t) =
    mconcat
      [ genericValidate net,
        declare "the text is not empty" (not (T.null t))
      ]

nonEmptyText :: Text -> Maybe NonEmptyText
nonEmptyText t = if T.null t then Nothing else Just (NonEmptyText t)

nonEmptyTextText :: NonEmptyText -> Text
nonEmptyTextText (NonEmptyText t) = t

instance HasCodec NonEmptyText where
  codec =
    named "NonEmptyText" $
      bimapCodec
        (maybe (Left "empty text where non-empty text was required") Right . nonEmptyText)
        nonEmptyTextText
        codec

data AnnotationPrecision
  = PrecisionFile
  | PrecisionDecl
  | PrecisionStatement !Span
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AnnotationPrecision)

instance Validity AnnotationPrecision

instance HasCodec AnnotationPrecision where
  codec =
    named "AnnotationPrecision" $
      dimapCodec fromEither toEither $
        disjointEitherCodec
          (stringConstCodec ((PrecisionFile, "file") :| [(PrecisionDecl, "decl")]))
          (object "PrecisionStatement" (requiredField "statement" "the statement it covers"))
    where
      fromEither = either id PrecisionStatement
      toEither = \case
        PrecisionStatement s -> Right s
        other -> Left other

-- | Two constructors rather than a magic string, so the adoption-debt
-- meta-check is a pattern match.
data Reason
  = ReasonAdoption
  | ReasonGiven !NonEmptyText
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Reason)

instance Validity Reason

-- | The one place this string is spelled, so nothing compares it by hand.
adoptionReason :: NonEmptyText
adoptionReason = NonEmptyText "pre-existing at adoption"

adoptionReasonText :: Text
adoptionReasonText = nonEmptyTextText adoptionReason

-- | A reason is its text. The distinction the two constructors draw is which
-- text it is, so nothing is gained by writing that distinction down twice.
instance HasCodec Reason where
  codec = named "Reason" (dimapCodec fromText toText codec)
    where
      fromText t = if t == adoptionReason then ReasonAdoption else ReasonGiven t
      toText = \case
        ReasonAdoption -> adoptionReason
        ReasonGiven t -> t

data AnnotationFact = AnnotationFact
  { annotationFactRule :: !RuleId,
    annotationFactScope :: !ScopeKey,
    annotationFactPrecision :: !AnnotationPrecision,
    annotationFactReason :: !Reason,
    annotationFactSpan :: !Span
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AnnotationFact)

instance Validity AnnotationFact

instance HasCodec AnnotationFact where
  codec =
    object "AnnotationFact" $
      AnnotationFact
        <$> requiredField "rule" "the rule it suppresses" .= annotationFactRule
        <*> requiredField "scope" "what it covers" .= annotationFactScope
        <*> requiredField "precision" "how precisely it is placed" .= annotationFactPrecision
        <*> requiredField "reason" "why" .= annotationFactReason
        <*> requiredField "span" "where the annotation itself is" .= annotationFactSpan

-- | A comment that meant to be a suppression and is not one. Carried in the
-- facts rather than raised at extraction time, because the project layer has
-- to see it too, and because a suppression that silently suppresses nothing is
-- the failure this whole mechanism exists to prevent.
data AnnotationProblem = AnnotationProblem
  { annotationProblemSpan :: !Span,
    annotationProblemMessage :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AnnotationProblem)

instance Validity AnnotationProblem

instance HasCodec AnnotationProblem where
  codec =
    object "AnnotationProblem" $
      AnnotationProblem
        <$> requiredField "span" "where the annotation is" .= annotationProblemSpan
        <*> requiredField "message" "what is wrong with it" .= annotationProblemMessage

-- | Stored whole through the codec it already carries, because a suppression is
-- read back as one thing or not at all.
instance PersistField AnnotationFact where
  toPersistValue = toPersistValue . TE.decodeUtf8 . LB.toStrict . JSON.encode . toJSONViaCodec
  fromPersistValue = fromPersistValueViaCodec

instance PersistFieldSql AnnotationFact where
  sqlType _ = SqlString
