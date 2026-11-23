module Hopinion.Check.Hs.OneLetPerBinding.Fact (CrowdedLet (..)) where

import Hopinion.Facts.Place

-- | One @let@ statement in a do-block that holds more than one binding.
--
-- The span covers the whole statement, because splitting it is what changes
-- and therefore what a suppression answers for.
data CrowdedLet = CrowdedLet
  { -- | How many bindings the statement holds, which is what tells two
    -- findings in one declaration apart.
    crowdedLetBindings :: !Word,
    crowdedLetSpan :: !Span,
    crowdedLetScope :: !ScopeKey
  }
