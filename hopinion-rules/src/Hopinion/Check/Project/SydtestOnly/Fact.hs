module Hopinion.Check.Project.SydtestOnly.Fact (ImportFact (..)) where

import Hopinion.Facts.Name
import Hopinion.Facts.Place

-- | One module a module imports, by the name its import wrote.
data ImportFact = ImportFact
  { importFactModule :: !ModuleKey,
    importFactSpan :: !Span
  }
