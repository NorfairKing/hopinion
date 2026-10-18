{-# LANGUAGE TemplateHaskell #-}

module Good where

import Data.Text (Text)
import Language.Haskell.TH (Dec, Q)

-- A bracket is an expression, and the constructor inside one is a template
-- rather than a declaration: nothing in this module has a constructor called
-- Quoted, nobody passes it an argument here, and a finding about it would
-- underline a bracket and ask for a newtype at a place where adding one fixes
-- nothing a reader of this file can see.
--
-- What a splice writes is beyond this rule in either direction, so the quote is
-- the half of it the rule can decline to read rather than read wrongly.
quoted :: Q [Dec]
quoted = [d|data Quoted = Quoted Text Text|]

-- Including when the quote is the whole of what the module is for, which is
-- how a module that generates declarations from data is written.
quotedPair :: Q [Dec]
quotedPair =
  [d|
    data QuotedPair = QuotedPair Text Int

    data QuotedAgain = QuotedAgain String String
    |]
