module WrittenRead where

newtype Port = Port Int

instance Read Port where
  readsPrec _ s = [(Port 0, s)]
