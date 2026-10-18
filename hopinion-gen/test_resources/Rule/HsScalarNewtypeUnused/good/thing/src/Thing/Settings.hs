module Thing.Settings where

import Data.Text (Text)
import Thing.Scalars (Height, Hostname)

-- The shape the rule asks for.
data Settings = Settings
  { settingsHostname :: !Hostname,
    settingsRowHeight :: !Height
  }

-- No newtype in this project wraps a Text under a name this field ends in, and
-- the rule says nothing. This is the whole point of reading the project's
-- lexicon rather than a word list: a field called text holding text is text.
data Row = Row
  { rowText :: !Text,
    rowMessage :: !Text
  }

-- Height wraps a Word and this is an Int, so the newtype is not the fix and
-- naming it would be wrong. A field that wants both a type and a different
-- primitive is a change nothing here can propose.
data Cursor = Cursor
  { cursorHeight :: !Int
  }

-- The match is on the tail of the field name, because that is where the record
-- prefix leaves the scalar. This field is a Text and Hostname wraps a Text, so
-- the suffix is the only thing keeping the rule quiet: a field with a hostname
-- in the middle of its name is about one rather than being one.
data Lookup = Lookup
  { lookupHostnamePattern :: !Text
  }

-- Nothing names this field, so it belongs to HsNoAnonymousScalar and not here.
-- One anonymous primitive is inside that rule's budget, so neither reports it,
-- which is the seam between the two worth having a case for.
data Resolved = Resolved !Text

-- The residue, said out loud: at a rendering boundary the scalar has stopped
-- being the domain type, and this is the bucket-C path that keeps that visible.
data TypstInput = TypstInput
  { -- [allow:HsScalarNewtypeUnused] Typst is handed the rendered form, so this
    -- is a Hostname that has already stopped being one.
    typstInputHostname :: !Text
  }
