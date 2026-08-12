module Thing where

-- TODO this one is left alone.
-- [allow:CommentBareTodo] Kept as an example of a suppressed comment finding.
loose :: Int
loose = 1

data Internal = Internal

-- [allow:TestGenValidSpecPerGenValid] This never crosses a process boundary,
-- it only exists for the debug endpoint's pretty printer, so there is nothing
-- worth generating. A reason runs to the end of its comment.
--
-- [allow:HsGenValidInGenPackage] Two rules at one site means two suppressions,
-- because the reasons differ.
instance GenValid Internal
