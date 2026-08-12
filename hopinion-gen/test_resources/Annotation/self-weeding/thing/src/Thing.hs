module Thing where

-- TODO one bare marker, answered by exactly one of the two below.
-- [allow:CommentBareTodo] The one that answers for it.
-- [allow:CommentBareTodo] Redundant, and reported as suppressing nothing.
covered :: Int
covered = 1

-- [allow:CommentBareTodo] There is nothing here to suppress.
pointless :: Int
pointless = 2

-- [allow:NoSuchRule] Names a rule that does not exist.
renamed :: Int
renamed = 3

-- [allow:CommentBareTodo]
silent :: Int
silent = 4

-- [allow] Does not name a rule at all.
bare :: Int
bare = 5
