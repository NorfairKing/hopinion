{-# LANGUAGE LambdaCase #-}

module Thing where

data Colour = Red | Green | Blue

data Mood
  = MoodHappy
  | MoodSad

warm :: Colour -> Bool
warm Red = True
warm _ = False

paint :: Mood -> Colour -> String
paint MoodHappy Red = "bright red"
paint _ Blue = "blue"
paint mood colour = seq mood (seq colour "grey")

pick :: Colour -> Colour -> Colour
pick = \cases
  Red Red -> Red
  _ other -> other
