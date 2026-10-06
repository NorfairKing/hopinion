{-# LANGUAGE GADTs #-}

module GoodWrittenAsAGadt where

data Shape where
  ShapeCircle :: Int -> Shape
  ShapeSquare :: Int -> Shape

data Event where
  EventOpened :: {eventOpenedAt :: Int} -> Event
