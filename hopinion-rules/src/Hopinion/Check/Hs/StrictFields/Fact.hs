module Hopinion.Check.Hs.StrictFields.Fact
  ( LazyFieldName (..),
    LazyField (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Hopinion.Facts.Place

-- | How a finding names the field it is about.
--
-- A record field has a name of its own, and one declaration can give several
-- names one type, which is still one field to annotate.
-- A positional field has only its place, so it is named by its constructor
-- and its position, counted from one.
data LazyFieldName
  = LazyFieldNameRecord !(NonEmpty Text)
  | LazyFieldNamePositional !Text !Word

-- | One field of a data declaration that is lazy.
--
-- The span is the field alone, because the field alone is what changes.
data LazyField = LazyField
  { lazyFieldName :: !LazyFieldName,
    lazyFieldSpan :: !Span,
    lazyFieldScope :: !ScopeKey
  }
