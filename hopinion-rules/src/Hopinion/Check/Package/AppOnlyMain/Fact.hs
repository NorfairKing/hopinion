{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hopinion.Check.Package.AppOnlyMain.Fact (StrayAppDecl (..)) where

import Data.Validity
import GHC.Generics (Generic)
import Hopinion.Facts.Name
import Hopinion.Facts.Place

-- | One declaration an executable's own source holds that is more than the
-- line an executable exists to write.
--
-- The name is what tells the two cases apart, so nothing here says which it
-- is: @main@ doing work of its own is a fact about @main@, and anything else
-- is a declaration that should not be there at all.
data StrayAppDecl = StrayAppDecl
  { strayAppDeclName :: !DeclName,
    strayAppDeclSpan :: !Span
  }
  deriving stock (Show, Eq, Generic)

instance Validity StrayAppDecl
