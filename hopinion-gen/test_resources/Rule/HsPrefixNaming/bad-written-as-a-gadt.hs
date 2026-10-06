{-# LANGUAGE GADTs #-}

module BadWrittenAsAGadt where

data Shape where
  Circle :: Int -> Shape
  ShapeSquare :: Int -> Shape

data Event where
  EventOpened :: {at :: Int} -> Event
