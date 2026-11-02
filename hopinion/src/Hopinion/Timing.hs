{-# LANGUAGE QuasiQuotes #-}

-- | How long each step of a run took, written beside its report.
--
-- Beside the report rather than into the build log, because a derivation's
-- output outlives its log: a cached build still has it, and a CI service that
-- substitutes every derivation shows nothing else.
--
-- Not part of the report: a report is compared across runs and across the two
-- ways of producing one, and a duration is never the same twice.
module Hopinion.Timing
  ( Timing (..),
    startClock,
    timed,
    forceList,
    timingFile,
    renderTimings,
  )
where

import Control.Exception (evaluate)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import Path (File, Path, Rel, relfile)
import Text.Printf (printf)

-- | One step and how long it took, in seconds of wall time.
--
-- Wall time rather than CPU time, because reading modules runs on several
-- cores and what a build waits for is the wall.
data Timing = Timing
  { timingStep :: !Text,
    timingSeconds :: !Double
  }

-- | Run an action and say how long it took.
--
-- Whatever the action leaves unevaluated is paid for by whoever forces it
-- later, so a caller that wants a step to own its cost forces the result
-- inside the action.
timed :: Text -> IO a -> IO (a, Timing)
timed step act = do
  stop <- startClock
  a <- act
  t <- stop step
  pure (a, t)

-- | Start a clock, and get back what reads it, for a step that runs in a monad
-- other than IO.
startClock :: IO (Text -> IO Timing)
startClock = do
  start <- getMonotonicTime
  pure $ \step -> do
    end <- getMonotonicTime
    pure
      Timing
        { timingStep = step,
          timingSeconds = end - start
        }

-- | Every element to weak head normal form, which is all of it for a type
-- whose fields are strict, so that the step that made a list pays for it.
forceList :: [a] -> IO ()
forceList = evaluate . foldl' (flip seq) ()

timingFile :: Path Rel File
timingFile = [relfile|timing.txt|]

-- | One line per step, the seconds first so that the column lines up, and a
-- tab between, so that @sort -n@ ranks the steps.
renderTimings :: [Timing] -> Text
renderTimings ts =
  T.unlines
    [ T.pack (printf "%.3f\t%s" (timingSeconds t) (T.unpack (timingStep t)))
    | t <- ts
    ]
