{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module CleanOrd where

data Colour = Red | Green
  deriving (Eq, Ord)

newtype Port = Port Int
  deriving stock (Eq, Ord)

newtype Retries = Retries Int
  deriving newtype (Eq, Ord)

newtype Weight = Weight Int
  deriving (Eq, Ord) via Int

data Tag = Tag String
  deriving (Eq)

deriving instance Ord Tag
