{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hopinion.Facts.Version
  ( FormatVersion (..),
    currentFormatVersion,
  )
where

import Data.Validity
import GHC.Generics (Generic)

newtype FormatVersion = FormatVersion {formatVersionNumber :: Word}
  deriving stock (Show, Eq, Ord, Generic)

instance Validity FormatVersion

-- | Bumped whenever a store this tool writes stops being one an older build of
-- it could read: a column added, a column reinterpreted, a table renamed.
--
-- The only version the store carries. A tool version beside it could not work:
-- the stamp is keyed on the format, so a merged store's differing tool version
-- would be dropped by the insert, and two versions where one is never read is a
-- check that reads as stronger than it is.
currentFormatVersion :: FormatVersion
currentFormatVersion = FormatVersion 4
