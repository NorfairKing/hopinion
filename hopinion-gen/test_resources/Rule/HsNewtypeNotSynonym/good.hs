module Good where

import Control.Monad.Reader (ReaderT)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL

-- What the rule asks for.
newtype UserId = UserId {unUserId :: Text}
  deriving stock (Show, Eq, Ord)

-- A phantom parameter on a newtype is what tells two of them apart.
newtype Key record = Key {unKey :: Int}
  deriving stock (Show, Eq, Ord)

-- A synonym for a compound type is shorthand for a shape.
type Users = Map UserId Text

type Handler = Text -> IO ()

type App = ReaderT Text IO

type Pair = (Int, Int)

type Names = [Text]

-- A parameter the right-hand side uses is a shape too.
type Id a = a

-- A flag is better as a type with two named constructors than as a newtype.
type IsAdmin = Bool

-- A synonym for an error that is prose is the error rule's to report, and it
-- asks for a sum type rather than a newtype.
type ParseError = Text

type LoadErrors = String

type ReadError = [Char]

type DecodeError = TL.Text

type RenderError = LText

-- A synonym for a type of the project's own names nothing new.
type Owner = UserId
