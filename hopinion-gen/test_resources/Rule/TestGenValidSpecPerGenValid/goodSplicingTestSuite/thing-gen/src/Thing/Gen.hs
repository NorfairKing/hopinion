{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Thing.Gen (genValidSpecsFor) where

import Language.Haskell.TH (Exp, Name, Q, conT)
import Test.Syd.Validity (genValidSpec)

genValidSpecsFor :: Name -> Q Exp
genValidSpecsFor name = [|genValidSpec @($(conT name))|]
