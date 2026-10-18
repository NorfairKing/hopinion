module Thing.Gitlab (ClientSecret (..)) where

import Data.Text (Text)

-- One forge's client secret is not another's, which is why a project ends up
-- with a type per forge under one name.
newtype ClientSecret = ClientSecret Text
