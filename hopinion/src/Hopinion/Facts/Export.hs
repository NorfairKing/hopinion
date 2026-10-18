{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | What a module says it exports.
module Hopinion.Facts.Export (ExportList (..), ExportEntry (..)) where

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
  = ExportListNone
  | ExportList !Span ![ExportEntry]
  deriving stock (Show, Eq, Generic)

instance Validity ExportList

-- | One thing an export list names, as the source wrote it.
--
-- Its own type because an entry is not prose: it is a name, sometimes with a
-- subordinate list after it, and a message quoting one back is quoting a name.
newtype ExportEntry = ExportEntry {unExportEntry :: Text}
  deriving stock (Show, Eq, Generic)

instance Validity ExportEntry
