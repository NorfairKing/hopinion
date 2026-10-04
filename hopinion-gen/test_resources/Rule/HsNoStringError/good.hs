{-# LANGUAGE LambdaCase #-}

module Good where

import Autodocodec (HasCodec (..), bimapCodec)
import Control.Monad.Except (ExceptT)
import Data.Text (Text)
import Data.Validation (Validation)
import qualified Data.Text as T
import Database.Persist (PersistField (..), PersistValue (..))

-- The failure is a type, so a caller can tell one from another, and the words
-- are chosen by whoever reports it rather than by whoever hit it.
data ParseError
  = NotANumber Text
  | OutOfRange Int Int

renderParseError :: ParseError -> Text
renderParseError = \case
  NotANumber t -> t
  OutOfRange _ _ -> "out of range"

parseAge :: Text -> Either ParseError Int
parseAge _ = Right 0

loadSetting :: Text -> ExceptT ParseError IO Int
loadSetting _ = pure 0

-- Short of its last argument, where the error is still a type of its own.
type Attempted = ExceptT ParseError IO

-- A String the function succeeds with is a result, and the rule is about the
-- argument that holds the failure.
describeAge :: Int -> Either ParseError String
describeAge _ = Right "young"

-- A Text that is the subject of a type rather than its failure.
newtype Greeting = Greeting Text

-- An instance method writes down no type of its own, so a prose failure the
-- class fixed is not one this module chose. Silent on purpose: what the rule
-- reads is the signatures we write, and there is none here to change.
instance PersistField Greeting where
  toPersistValue (Greeting t) = PersistText t
  fromPersistValue pv = do
    t <- fromPersistValue pv
    if T.null t
      then Left "a greeting cannot be empty"
      else Right (Greeting t)

-- Same for a function handed to a combinator, where the failure belongs to the
-- combinator and is written nowhere here. Naming this lambda and giving it a
-- signature is what would put the type in our hands, and would be reported.
instance HasCodec Greeting where
  codec =
    bimapCodec
      ( \t ->
          if T.null t
            then Left "a greeting cannot be empty"
            else Right (Greeting t)
      )
      (\(Greeting t) -> t)
      codec

-- Nothing fixes which argument of a Validation is the failure, and a type that
-- put its success first would read the same way. What tells the orders apart is
-- that this one accumulates, so prose on its own here is left alone: the only
-- Validation that would write it is an Either with more syllables.
single :: Validation Text Int
single = pure 0

spelledOutSingle :: Validation [Char] Int
spelledOutSingle = pure 0

-- Constructors are the point: these are what a caller branches on, and what a
-- renderer turns into words at the place that knows who is reading.
data LookupError
  = NoSuchKey Text
  | KeyExpired Int

-- One constructor is still nothing to branch on, so a wrapper has to wrap
-- something that is already a type.
newtype StoreError = StoreError LookupError

-- A name that does not call itself an error is not read as one, which is the
-- cost of reading the name at all.
newtype Complaint = Complaint Text

-- A constructor that names the failure holds what was not understood rather
-- than words about it, and the words come from the renderer that matches on the
-- name. One constructor is enough when the name is the information.
--
-- What decides that is the renderer, which is not here to be read: a renderer
-- that hands its payload straight back is the reported shape wearing a
-- constructor name that does not echo, and this rule cannot tell the two apart.
-- It reports the wrapper it can be sure of and leaves this one alone, so what
-- it misses is a type named better than it is.
newtype RepoTypeParseError = UnknownRepoType Text

renderRepoTypeParseError :: RepoTypeParseError -> Text
renderRepoTypeParseError = \case
  UnknownRepoType t -> T.concat ["Unknown repo type: ", t]
