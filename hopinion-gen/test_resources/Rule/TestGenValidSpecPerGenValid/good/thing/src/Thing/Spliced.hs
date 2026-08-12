{-# LANGUAGE TemplateHaskell #-}

module Thing.Spliced where

import Language.Haskell.TH

-- The engine cannot see what the splice generates, so it abstains for this
-- module. The instance written out here is visible all the same, and tested.
data Generated = Generated

instance GenValid Generated

$(pure [])
