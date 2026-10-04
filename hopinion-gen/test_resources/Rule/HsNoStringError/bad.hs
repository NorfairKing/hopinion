module Bad where

import Control.Monad.Except (Except, ExceptT, MonadError, throwError)
import qualified Control.Monad.Except as Except
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validation (Validation)

-- The failure of an Either is its first argument, and here it is prose.
parseAge :: String -> Either String Int
parseAge s = case reads s of
  [(n, "")] -> Right n
  _ -> Left "not a number"

-- Text is prose as much as String is.
loadSetting :: Text -> ExceptT Text IO Int
loadSetting _ = pure 0

-- A list of messages is a list of prose, and the accumulating shapes are
-- where it gets written.
validateAll :: [Int] -> Either [String] [Int]
validateAll = Right

firstFailure :: Either (NonEmpty Text) Int
firstFailure = Right 0

-- A record field says what a type is as much as a function's signature does.
data Attempt = Attempt
  { attemptName :: !Text,
    attemptOutcome :: !(Either String Int)
  }

-- And a constraint says it about every failure the monad can have.
refuse :: (MonadError String m) => m a
refuse = throwError "no"

-- [Char] is String spelled out.
spelledOut :: Either [Char] Int
spelledOut = Right 0

-- Short of its last argument, which does not change which argument holds the
-- failure.
type Attempted = ExceptT String IO

-- Two carriers in one type are two failures to fix, and neither is the other's
-- argument.
bothWays :: Either String (Except Text Int)
bothWays = Right (pure 0)

-- The names a type and its argument are written under say nothing about which
-- argument carries the failure.
qualifiedCarrier :: Except.ExceptT T.Text IO Int
qualifiedCarrier = pure 0

-- Accumulated rather than one at a time, and prose either way.
collected :: Validation [Text] Int
collected = pure 0

-- A NonEmpty accumulates as much as a list does.
collectedAtLeastOne :: Validation (NonEmpty String) Int
collectedAtLeastOne = pure 0

-- Naming the prose does not make it a type.
type ParseError = Text

-- Nor does wrapping it: one constructor is nothing to branch on.
newtype LookupError = LookupError String

-- A data declaration with one constructor is a newtype with more keystrokes.
data RenderError = RenderError Text

-- Named for what it holds, and holding prose.
type ValidationErrors = NonEmpty Text

-- A field name does not change what the constructor holds.
newtype StoreError = StoreError {unStoreError :: Text}
