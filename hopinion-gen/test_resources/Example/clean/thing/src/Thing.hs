{-# LANGUAGE DeriveGeneric #-}

module Thing where

import Data.Validity (Validity)
import GHC.Generics (Generic)

-- TODO a finding that a suppression answers for, so a Nix run exercises the
-- annotation path too and not only the check.
-- [allow:CommentBareTodo] Kept so this project stays clean through a
-- suppression rather than by having nothing to suppress.
data Widget = Widget
  deriving (Show, Generic)

instance Validity Widget
