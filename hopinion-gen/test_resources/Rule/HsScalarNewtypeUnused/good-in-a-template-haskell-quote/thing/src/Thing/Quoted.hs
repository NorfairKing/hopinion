{-# LANGUAGE TemplateHaskell #-}

module Thing.Quoted where

import Data.Text (Text)
import Language.Haskell.TH (Dec, Q)

-- Nothing in this package declares a type called Quoted, so there is no type
-- for the field below to have been given: reading a quoted newtype into the
-- lexicon would have this rule name a type that cannot be imported because it
-- does not exist.
quoted :: Q [Dec]
quoted = [d|newtype Quoted = Quoted Text|]

data Holder = Holder
  { holderQuoted :: !Text
  }

-- The other direction, and silent for the same reason: the field inside the
-- bracket is a template rather than a field, and a finding about it would
-- underline a bracket.
quotedField :: Q [Dec]
quotedField =
  [d|
    data QuotedHolder = QuotedHolder {quotedHolderHolder :: Text}
    |]
