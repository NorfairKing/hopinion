{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module CleanEq where

data Colour = Red | Green
  deriving (Eq)

newtype Port = Port Int
  deriving stock (Eq)

newtype Retries = Retries Int
  deriving newtype (Eq)

newtype Weight = Weight Int
  deriving (Eq) via Int

data Tag = Tag String

deriving instance Eq Tag
