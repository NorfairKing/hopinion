{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Facts.Component
  ( ComponentKind (..),
    componentKindText,
    parseComponentKind,
  )
where

import Data.Text (Text)
import Data.Validity
import Database.Persist (PersistField (..))
import Database.Persist.Sql (PersistFieldSql (..), SqlType (..))
import GHC.Generics (Generic)

data ComponentKind
  = ComponentLib
  | ComponentApp
  | ComponentTest
  | ComponentBench
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance Validity ComponentKind

instance PersistField ComponentKind where
  toPersistValue = toPersistValue . componentKindText
  fromPersistValue v = do
    t <- fromPersistValue v
    maybe (Left "not a component kind") Right (parseComponentKind t)

instance PersistFieldSql ComponentKind where
  sqlType _ = SqlString

componentKindText :: ComponentKind -> Text
componentKindText = \case
  ComponentLib -> "lib"
  ComponentApp -> "app"
  ComponentTest -> "test"
  ComponentBench -> "bench"

-- | What @--component@ accepts as well as what a row stores, so the spellings a
-- person can type are the spellings the store uses and there is one table of
-- them rather than two.
parseComponentKind :: Text -> Maybe ComponentKind
parseComponentKind t = lookup t [(componentKindText k, k) | k <- [minBound .. maxBound]]
