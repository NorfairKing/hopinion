{-# LANGUAGE OverloadedStrings #-}

module Good where

import Data.Text (Text)
import qualified Data.Text as T

-- The pieces in a list, which is what the rule asks for.
located :: Text -> Text -> Text
located path message = T.pack (unwords ["at", T.unpack path, T.unpack message])

-- The operator on something that is not a string, which is what it is for.
firstOf :: Maybe Text -> Maybe Text -> Maybe Text
firstOf a b = a <> b

-- Defining the operator is not using it, however many strings are in reach.
newtype Longest = Longest Text

instance Semigroup Longest where
  Longest a <> Longest b = Longest (if T.length a < T.length b then b else a)

-- A string literal with no operator anywhere near it.
marker :: Text
marker = "-->"

-- Appending lists that are not strings, which is what ++ is for.
digits :: [Char]
digits = ['a' .. 'z'] ++ ['0' .. '9']
