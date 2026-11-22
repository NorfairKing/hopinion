module Hopinion.Check.Hs.RecordFieldPerLine.Fact
  ( RecordUse (..),
    CrowdedRecord (..),
  )
where

import Hopinion.Facts.Place

-- | The two ways a record value is written.
--
-- Two rather than one because a reader is told which one this is before they
-- look: a construction is named by its constructor, and an update is a brace
-- somewhere to the right of the value it changes.
data RecordUse
  = RecordUseConstructed
  | RecordUseUpdated

-- | One record value that puts two of its fields on one line.
--
-- The span covers the whole expression, braces included, because the whole of
-- it is what changes and therefore what a suppression answers for.
data CrowdedRecord = CrowdedRecord
  { crowdedRecordUse :: !RecordUse,
    -- | How many fields are written out, which is what tells two findings in
    -- one declaration apart.
    crowdedRecordFields :: !Word,
    crowdedRecordSpan :: !Span,
    crowdedRecordScope :: !ScopeKey
  }
