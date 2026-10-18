module Thing.Settings where

import Data.Text (Text)

-- The field names the scalar and the type does not, while the type that would
-- is two modules away in the same package.
data Settings = Settings
  { settingsHostname :: !Text,
    -- A container is transparent: the fix is Maybe Username.
    settingsUsername :: !(Maybe Text)
  }

-- Both Name and OwnerName match the tail of this field's name, and the longest
-- match is the one a reader can act on.
data Repo = Repo
  { repoOwnerName :: !Text
  }
