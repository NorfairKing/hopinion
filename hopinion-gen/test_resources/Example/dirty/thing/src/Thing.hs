module Thing where

-- TODO a bare marker, which is the module-level finding.
data Widget = Widget

-- A generator outside a -gen package, which is the package-level finding, and
-- an obligation the gen package's test suite does not meet, which is the
-- project-level one.
instance GenValid Widget

-- The comment below is what the rule the example executable registers is
-- about, and no shipped rule minds it. It is its own block, because a block is
-- judged as one comment and a paragraph of ordinary prose beside it would not
-- be shouting.

-- SHOUTING, WHICH NO SHIPPED RULE MINDS.
newtype Loud = Loud Int
