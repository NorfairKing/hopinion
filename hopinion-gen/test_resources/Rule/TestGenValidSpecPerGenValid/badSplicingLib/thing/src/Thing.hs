{-# LANGUAGE TemplateHaskell #-}

module Thing where

import Language.Haskell.TH

-- What the splice adds is unknowable, but this instance is written out, and
-- nothing in the gen package tests it.
data Visible = Visible

instance GenValid Visible

$(pure [])
