{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | What a module says it exports.
module Hopinion.Facts.Export (ExportList (..)) where

import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)
import Hopinion.Facts.Place

-- | Two answers rather than a list that can be empty: a module with no export
-- list exports everything it defines, and one with an empty list exports
-- nothing, so the absent list is not the empty one.
--
-- The entries are the text the source wrote, which is what a message about
-- them has to say back. Documentation in an export list is not one of them: a
-- section heading exports nothing.
data ExportList
  = NoExportList
  | ExportList !Span ![Text]
  deriving stock (Show, Eq, Generic)

instance Validity ExportList
