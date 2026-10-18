module Thing.Github (ClientSecret (..)) where

import Data.Text (Text)

newtype ClientSecret = ClientSecret Text
