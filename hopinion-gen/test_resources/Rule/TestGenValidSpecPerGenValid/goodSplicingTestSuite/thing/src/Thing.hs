module Thing where

-- Nothing written anywhere calls the combinator for this type, and nothing can
-- tell whether the splice in the gen package's test suite does.
data Visible = Visible

instance GenValid Visible
