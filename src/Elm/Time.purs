
-- | Library for working with time

module Elm.Time
    ( module Virtual
    , Time, toTime, fromTime
    , now, every
    , millisecond, second, minute, hour
    , inMilliseconds, inSeconds, inMinutes, inHours
    ) where


-- For re-export

import Control.Monad.Aff (effCanceler, liftEff')
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Now as Now
import Control.Monad.Eff.Timer (setInterval, clearInterval) as Timer
import Control.Monad.Except.Trans (ExceptT(..))
import Control.Monad.IO (launchIO)
import Control.Monad.IOSync (runIOSync')
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Int (round)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Hours(..), Minutes(..), Seconds(..), Milliseconds(..), class Duration, toDuration, fromDuration)
import Data.Time.Duration (class Duration) as Virtual
import Data.Tuple.Nested ((/\))
import Elm.Basics (Float, Never, (|>))
import Elm.Dict as Dict
import Elm.List as List
import Elm.Platform (Manager, Task, subscription)
import Elm.Platform as Platform
import Elm.Platform.Sub (Sub)
import Elm.Process (kill, spawn)
import Elm.Task (makeTask, toIO)
import Elm.Task as Task
import Prelude (class Functor, Unit, bind, flip, id, pure, unit, ($), (/), (<$>), (<<<))
import Type.Prelude (Proxy)


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
now :: ∀ x. Task x Time
now =
    ExceptT $ (Right <<< toTime <<< unInstant) <$> (liftAff <<< liftEff') Now.now


-- | Subscribe to the current time. First you provide an interval describing how
-- | frequently you want updates. Second, you give a tagger that turns a time into a
-- | message for your `update` function. So if you want to hear about the current
-- | time every second, you would say something like this:
-- |
-- |     type Msg = Tick Time | ...
-- |
-- |     subscriptions model =
-- |       every second Tick
-- |
-- | Check out the [Elm Architecture Tutorial][arch] for more info on how
-- | subscriptions work.
-- |
-- | [arch]: https://github.com/evancz/elm-architecture-tutorial/
-- |
-- | **Note:** this function is not for animation! You need to use something based
-- | on `requestAnimationFrame` to get smooth animations. This is based on
-- | `setInterval` which is better for recurring tasks like “check on something
-- | every 30 seconds”.
every :: ∀ msg. Time -> (Time -> msg) -> Sub msg
every interval tagger =
    subscription timeManager (Every interval tagger)


-- SUBSCRIPTIONS


data MySub msg =
    Every Time (Time -> msg)


instance functorMySub :: Functor MySub where
    map f (Every interval tagger) =
        Every interval (f <<< tagger)


-- EFFECT MANAGER

timeManager :: Manager Proxy MySub Time State
timeManager = {init, onEffects, onSelfMsg, tag}


-- A unique tag for our effect manager. (Ideally, we change things eventually
-- so we don't need this).
tag :: String
tag = "Elm.Time"


newtype State appMsg =
    State
        { taggers :: Taggers appMsg
        , processes :: Processes
        }


type Processes =
    Dict.Dict Time Platform.ProcessId


type Taggers appMsg =
    Dict.Dict Time (List (Time -> appMsg))


init :: ∀ appMsg. Task Never (State appMsg)
init =
    pure $ State
        { taggers: Dict.empty
        , processes: Dict.empty
        }


onEffects :: ∀ msg. Platform.Router msg Time -> List (Proxy msg) -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router _ subs (State {processes}) =
    let
        newTaggers =
            List.foldl addMySub Dict.empty subs

        leftStep interval taggers (spawnList /\ existingDict /\ killTask) =
            ((interval : spawnList) /\ existingDict /\ killTask)

        bothStep interval taggers id (spawnList /\ existingDict /\ killTask) =
            (spawnList /\ Dict.insert interval id existingDict /\ killTask)

        rightStep _ id (spawnList /\ existingDict /\ killTask) =
            ( spawnList
            /\ existingDict
            /\ (kill id |> Task.andThen (\_ -> killTask))
            )

        (spawnList /\ existingDict /\ killTask) =
            Dict.merge
                leftStep
                bothStep
                rightStep
                newTaggers
                processes
                (Nil /\ Dict.empty /\ Task.succeed unit)
    in
        killTask
            |> Task.andThen (\_ -> spawnHelp router spawnList existingDict)
            |> Task.andThen (\newProcesses -> Task.succeed $ State {taggers: newTaggers, processes: newProcesses})


addMySub :: ∀ msg. MySub msg -> Taggers msg -> Taggers msg
addMySub (Every interval tagger) state =
    case Dict.get interval state of
        Nothing ->
            Dict.insert interval (List.singleton tagger) state

        Just taggers ->
            Dict.insert interval (tagger : taggers) state


spawnHelp :: ∀ x msg. Platform.Router msg Time -> List Time -> Processes -> Task.Task x Processes
spawnHelp router intervals processes =
    case intervals of
        Nil ->
            Task.succeed processes

        interval : rest ->
            let
                spawnTimer =
                    spawn (setInterval interval (Platform.sendToSelf router interval))

                spawnRest id =
                    spawnHelp router rest (Dict.insert interval id processes)
            in
                spawnTimer
                    |> Task.andThen spawnRest


onSelfMsg :: ∀ msg. Platform.Router msg Time -> Time -> State msg -> Task Never (State msg)
onSelfMsg router interval state@(State {taggers}) =
    case Dict.get interval taggers of
        Nothing ->
            Task.succeed state

        Just intervalTaggers ->
            let
                tellTaggers time =
                    Task.sequence (List.map (\tagger -> Platform.sendToApp router (tagger time)) intervalTaggers)
            in
                now
                    |> Task.andThen tellTaggers
                    |> Task.andThen (\_ -> Task.succeed state)


-- | Creates a task which, when run, will launch the provided task repeatedly,
-- | forever.
-- |
-- | Well, unless, when running the task which this function returns, you keep
-- | track of the `ProcessID` ... it will stop launching the provided task when
-- | you kill it.
setInterval :: ∀ x. Time -> Task Never Unit -> Task x Never
setInterval interval task =
    -- We never call back, as the interval can only be canceled ... it never completes.
    makeTask \_ -> do
       intervalId <- Timer.setInterval (round interval) (runIOSync' $ launchIO $ toIO task)
       pure $ effCanceler (Timer.clearInterval intervalId)
