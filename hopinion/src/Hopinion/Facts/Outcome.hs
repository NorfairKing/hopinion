{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Facts.Outcome (ParseOutcome (..)) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Validity
import Data.Validity.Text ()
import Database.Persist (PersistField (..))
import Database.Persist.Sql (PersistFieldSql (..), SqlType (..))
import GHC.Generics (Generic)
import Hopinion.Facts.Persist
import Hopinion.Facts.Place

-- | Kept in the facts rather than thrown away, because the project layer must
-- treat an unparsed module as an error rather than as a module with no
-- declarations.
data ParseOutcome
  = ParsedOk
  | ParseFailed !Position !Text
  | -- | The module is real and its source is not Haskell: a preprocessor
    -- generates it at build time. Neither a parse failure nor a module that
    -- vanished, so it must be neither reported nor silently dropped.
    NotHaskellSource
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ParseOutcome)

instance Validity ParseOutcome

instance HasCodec ParseOutcome where
  codec =
    named "ParseOutcome" $
      dimapCodec fromEither toEither $
        disjointEitherCodec
          (stringConstCodec ((ParsedOk, "ok") :| [(NotHaskellSource, "not-haskell-source")]))
          ( object "ParseFailed" $
              (,)
                <$> requiredField "loc" "where the parse failed" .= fst
                <*> requiredField "message" "why the parse failed" .= snd
          )
    where
      fromEither = either id (uncurry ParseFailed)
      toEither = \case
        ParsedOk -> Left ParsedOk
        NotHaskellSource -> Left NotHaskellSource
        ParseFailed l m -> Right (l, m)

-- | A shape rather than a spelling, and nothing queries into it, so it is
-- stored whole through the codec it already carries.
instance PersistField ParseOutcome where
  toPersistValue = toPersistValue . TE.decodeUtf8 . LB.toStrict . JSON.encode . toJSONViaCodec
  fromPersistValue = fromPersistValueViaCodec

instance PersistFieldSql ParseOutcome where
  sqlType _ = SqlString
