module Thing.Scalars where

import Data.Text (Text)

-- A newtype's own accessor is the declaration, not a site that declined it.
newtype Hostname = Hostname {unHostname :: Text}

newtype Height = Height Word

-- A wrapper around a project type is not a scalar newtype, so nothing about it
-- goes in the lexicon.
newtype Endpoint = Endpoint Hostname
