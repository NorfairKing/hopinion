{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hopinion.Facts.Occurrence (NameFact (..)) where

import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)
import Hopinion.Facts.Place

-- | A capitalised name as the module's own tokens have it, and where an
-- annotation about it has to go.
--
-- Read off the tokens rather than off the parse tree, because the tokens are
-- where a name written in the code is already told apart from the same word in
-- a comment, in a string literal, or in the tail of a module name.
data NameFact = NameFact
  { nameFactText :: !Text,
    nameFactSpan :: !Span,
    nameFactScope :: !ScopeKey
  }
  deriving stock (Show, Eq, Generic)

instance Validity NameFact
