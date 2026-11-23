-- | A project's own prelude, naming the scalars it imports rather than domain
-- values.
module Good.Prelude where

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy as TL

type LText = TL.Text

type LByteString = LB.ByteString
