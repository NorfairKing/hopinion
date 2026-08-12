module Fixture where

run :: IO ()
run = do
  {- The statement below is what this is about,
     and this second line starts with neither dashes
     nor a brace, which is the whole point. -}
  let x = 1
  print x
