{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module GoodUnlifted where

import GHC.Exts (Array#, Int#)

data Boxed = Boxed Int#

data Both = Both
  { bothPair :: (# Int#, Int# #),
    bothEither :: (# Int# | () #),
    bothArray :: Array# Int
  }
