{-# LANGUAGE TemplateHaskell #-}

-- | What the splice is made of. A module of its own, because a quotation is
-- resolved where it is written and a splice cannot run code from its own
-- module.
module Generator (Widget (..), Gennable, probe, spliceInstance, spliceCall) where

import Language.Haskell.TH (Dec, Exp, Q)

data Widget = Widget

class Gennable a

probe :: Widget -> ()
probe _ = ()

spliceInstance :: Q [Dec]
spliceInstance = [d|instance Gennable Widget|]

spliceCall :: Q Exp
spliceCall = [|probe Widget|]
