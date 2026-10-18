-- | 'Width' is exported without its constructor on purpose: the named widths
-- are then the only ones a panel can have, so one the grid has no room for is
-- a thing nobody can write rather than a thing something has to check.
module Thing.Grid
  ( Width,
    unWidth,
    halfWidth,
    fullWidth,
  )
where

newtype Width = Width {unWidth :: Word}

halfWidth :: Width
halfWidth = Width 12

fullWidth :: Width
fullWidth = Width 24
