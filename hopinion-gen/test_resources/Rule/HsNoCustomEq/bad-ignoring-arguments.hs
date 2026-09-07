module BlindEq where

data Handle = Handle Int

-- Show and Read let an instance off when its methods discard the value, for
-- keeping a secret out of the logs. Equality does not get that exception.
instance Eq Handle where
  _ == _ = True
