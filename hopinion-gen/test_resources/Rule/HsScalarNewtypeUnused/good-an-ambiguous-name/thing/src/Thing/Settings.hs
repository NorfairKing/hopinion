module Thing.Settings where

import Data.Text (Text)

-- Two modules declare a ClientSecret, so the name names no type and there is
-- nothing this rule could tell a reader to use: naming one of them would be
-- picking between them, which is a choice the rule has no basis for making.
data Settings = Settings
  { settingsClientSecret :: !Text
  }
