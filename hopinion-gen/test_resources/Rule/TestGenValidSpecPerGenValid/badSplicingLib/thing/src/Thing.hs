{-# LANGUAGE TemplateHaskell #-}

module Thing where

import Data.GenValidity (GenValid)
import Data.Validity (Validity)
import GHC.Generics (Generic)
import Language.Haskell.TH

-- What the splice adds is unknowable, but this instance is written out, and
-- nothing in the gen package tests it.
data Visible = Visible
  deriving (Show, Generic)

instance Validity Visible

instance GenValid Visible

$(pure [])
