{-# LANGUAGE OverloadedStrings #-}

module BadBehindDollar where

import Data.Text (Text)
import qualified Data.Text as T

-- Behind a $, where the tree the parser builds hangs the literal off the $
-- rather than off the concatenation.
shouted :: Text -> Text
shouted name = T.toUpper $ "hello " <> name

-- The same with ++, which is where this shape is most common.
report :: String -> IO ()
report what = putStrLn $ "no such thing: " ++ what
