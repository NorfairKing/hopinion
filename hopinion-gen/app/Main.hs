module Main (main) where

import Hopinion (hopinionWith)
import Hopinion.Rule.Gen (exampleRules)

main :: IO ()
main = hopinionWith exampleRules
