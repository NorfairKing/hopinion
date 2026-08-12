module Thing where

-- TODO a finding nothing answers for.
plain :: Int
plain = 1

-- TODO a leading marker.
-- [allow:CommentBareTodo] One suppression, two findings, which is too broad.
both :: Int
both = 2 -- TODO a trailing marker.

-- [allow:CommentBareTodo] There is nothing here to suppress.
pointless :: Int
pointless = 3

-- [allow:NoSuchRule] Names a rule that does not exist.
renamed :: Int
renamed = 4
