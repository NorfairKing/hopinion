{-# LANGUAGE OverloadedStrings #-}

module Bad where

import Data.Text (Text)

-- A literal on the left.
greeting :: Text -> Text
greeting name = "Hello, " <> name

-- And on the right, which is the same concatenation the other way around.
sentence :: Text -> Text
sentence body = body <> "."

-- A chain is one thing to fix, so it is one finding rather than three.
located :: Text -> Text -> Text
located path message = "at " <> path <> ": " <> message

-- Parenthesised, which does not make it two chains either.
label :: Text -> Text
label t = ("[" <> t) <> "]"

-- Both operands literals, which is the shape that reads most like it wanted a
-- list all along.
marker :: Text
marker = "--" <> ">"

-- The other operator, which concatenates the same things.
quoted :: String -> String
quoted s = "\"" ++ s ++ "\""

-- Both operators in one expression, which is still one list to write.
mixed :: String -> String
mixed s = "at " ++ s <> "."
