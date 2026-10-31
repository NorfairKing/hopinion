{-# LANGUAGE TemplateHaskell #-}

-- | Nothing here names @Gennable@, and nothing here names @probe@ at a
-- @Widget@. Both are in the compiled module all the same, which is the whole
-- reason these files are read.
module Fixture where

import Generator

data Written = Written
  deriving (Show)

written :: Written
written = Written

$(spliceInstance)

called :: ()
called = $(spliceCall)
