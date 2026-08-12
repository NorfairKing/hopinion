-- | Reading a fact back out of a column it was stored in whole, through the
-- codec it already carries, so there is one encoding of it rather than two.
--
-- Here rather than beside either caller because the plain facts and the
-- suppression ones both need it and neither can import the other.
--
-- The failure is a 'Text' rather than a type of its own because this implements
-- @persistent@'s 'fromPersistValue', so the shape is the library's.
module Hopinion.Facts.Persist (fromPersistValueViaCodec) where

import Autodocodec
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist

fromPersistValueViaCodec :: (HasCodec a) => PersistValue -> Either Text a
fromPersistValueViaCodec v = do
  t <- fromPersistValue v
  case JSON.eitherDecode (LB.fromStrict (TE.encodeUtf8 t)) of
    Left err -> Left (T.pack err)
    Right value -> first T.pack (JSON.parseEither parseJSONViaCodec value)
