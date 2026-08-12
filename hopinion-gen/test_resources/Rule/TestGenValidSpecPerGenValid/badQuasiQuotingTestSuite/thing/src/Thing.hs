module Thing where

-- Nothing calls the combinator for this type, and the quasiquote in the gen
-- package's test suite expands a body written out there, so it cannot be the
-- call that is missing.
data Visible = Visible

instance GenValid Visible
