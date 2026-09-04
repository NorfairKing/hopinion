{-# LANGUAGE TypeApplications #-}

module HiddenSecrets where

newtype ClientSecret = ClientSecret String

instance Show ClientSecret where
  show _ = show @String "<client secret>"

newtype ApiKey = ApiKey String

instance Show ApiKey where
  showsPrec _ _ = showString "<api key>"

newtype Unreadable = Unreadable String

instance Read Unreadable where
  readsPrec _ _ = []
