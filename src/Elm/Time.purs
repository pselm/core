
-- | Library for working with time

module Elm.Time
    ( module Virtual
    , Time, toTime, fromTime
    , now --, every
    , millisecond, second, minute, hour
    , inMilliseconds, inSeconds, inMinutes, inHours
    ) where


-- For re-export

import Data.Time.Duration (class Duration) as Virtual


-- Internal

import Data.Time.Duration
    ( Hours(..), Minutes(..), Seconds(..), Milliseconds(..)
    , class Duration, toDuration, fromDuration
    )

import Elm.Basics (Float, Bool)
import Elm.Types (Task, TaskE)

import Control.Monad.Except.Trans (ExceptT(..))
import Control.Monad.Aff (liftEff')
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Now as Now
import Data.Either (Either(..))
import Data.Int (round)
import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(..))

import Prelude ((/), flip, id, ($), (<$>), (<<<), bind, pure, (-), unit, (>>=), void, const, negate, (+), (/=))


-- | Type alias to make it clearer when you are working with time values.
-- | Using the `Time` helpers like `second` and `inSeconds` instead of raw numbers
-- | is very highly recommended.
-- |
-- | Note that Purescript's `Data.Time` class does something similar with its
-- | `Duration` class, which has separate types for `Hours`, `Minutes`, `Seconds`
-- | and `Milliseconds`.
type Time = Number 


-- | Convert any of Purescript's `Duration` types to `Time`.
toTime :: forall a. (Duration a) => a -> Time
toTime tv =
    case fromDuration tv of
         Milliseconds n -> n


-- | Convert from `Time` to any of Purescript's `Duration` types.
fromTime :: forall a. (Duration a) => Time -> a
fromTime =
    toDuration <<< Milliseconds


-- | Units of time, making it easier to specify things like a half-second
-- | `(500 * millisecond)` without remembering Elm&rsquo;s underlying units of time.
millisecond :: Time
millisecond =
    toTime $ Milliseconds 1.0


second :: Time 
second =
    toTime $ Seconds 1.0


minute :: Time
minute =
    toTime $ Minutes 1.0


hour :: Time 
hour =
    toTime $ Hours 1.0


divBy :: Number -> Number -> Number
divBy = flip (/)


inMilliseconds :: Time -> Float
inMilliseconds = id


inSeconds :: Time -> Float
inSeconds = divBy second


inMinutes :: Time -> Float
inMinutes = divBy minute


inHours :: Time -> Float
inHours = divBy hour


-- | Get the `Time` at the moment when this task is run.
now :: ∀ e x. TaskE (now :: NOW | e) x Time
now =
    ExceptT $ (Right <<< toTime <<< unInstant) <$> liftEff' Now.now
    
