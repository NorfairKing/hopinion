{-# LANGUAGE TypeFamilies #-}

module Thing where

data family Key a

data instance Key Int
  = KeyOne
  | KeyTwo

isOne :: Key Int -> Bool
isOne KeyOne = True
isOne _ = False

type IntKey = Key Int

isTwo :: IntKey -> Bool
isTwo KeyTwo = True
isTwo _ = False
