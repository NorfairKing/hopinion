module Thing.Scalars where

import Data.Text (Text)

-- The project's own lexicon, which is the only one the rule reads.
newtype Hostname = Hostname Text

newtype Username = Username Text

newtype InstallationId = InstallationId Int

-- Two that both match a field ending in Name, so the rule has to choose.
newtype Name = Name Text

newtype OwnerName = OwnerName Text
