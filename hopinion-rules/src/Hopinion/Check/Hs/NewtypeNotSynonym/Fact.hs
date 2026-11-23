module Hopinion.Check.Hs.NewtypeNotSynonym.Fact (ScalarSynonym (..)) where

import Data.Text (Text)
import Hopinion.Facts.Place

-- | A synonym for one scalar type.
--
-- The span is the whole declaration, because the whole declaration is what
-- changes: the keyword, a constructor, and a field to unwrap with.
data ScalarSynonym = ScalarSynonym
  { scalarSynonymName :: !Text,
    -- | The scalar's name without its qualifier, so a finding names what a
    -- reader sees on the line, except that @[Char]@ is named @String@.
    scalarSynonymScalar :: !Text,
    scalarSynonymSpan :: !Span,
    scalarSynonymScope :: !ScopeKey
  }
