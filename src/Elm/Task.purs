
-- | Tasks make it easy to describe asynchronous operations that may fail,
-- | like HTTP requests or writing to a database.
-- |
-- | This is implemented on top of Purescript's `Aff` type. The main difference
-- | is that `Task` has a polymorphically-typed error channel, whereas
-- | the error channel for `Aff` can only represent a `String`.

module Elm.Task
    ( module Virtual
    , toIO, fromIO
    , makeTask
    , EffFnTask, fromEffFnTask
    , succeed, fail
    , mapError, onError
    , perform, attempt
    , toMaybe, fromMaybe
    , toResult, fromResult
    , ThreadID
    ) where


import Control.Monad.Aff (Canceler(Canceler), Error, makeAff, nonCanceler)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Compat (EffFnCanceler(..), EffFnCb)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn3, mkEffFn1, runEffFn3)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, withExceptT)
import Control.Monad.IO (IO)
import Data.Either (Either(..), either)
import Data.List (List)
import Data.Traversable (sequence)
import Data.Traversable (sequence) as Virtual
import Elm.Apply (andMap, map2, map3, map4, map5) as Virtual
import Elm.Basics (Never, (|>))
import Elm.Bind (andThen)
import Elm.Bind (andThen) as Virtual
import Elm.List as List
import Elm.Maybe (Maybe(..))
import Elm.Platform (Manager, ProcessId, Task, command)
import Elm.Platform (Task) as Virtual
import Elm.Platform as Platform
import Elm.Platform.Cmd (Cmd)
import Elm.Process (spawn)
import Elm.Result (Result(..))
import Prelude (class Functor, Unit, bind, const, discard, map, pure, unit, ($), (<$>), (<<<), (>>=))
import Prelude (map) as Virtual
import Type.Prelude (Proxy)


-- | Takes a `Task` and unwraps the underlying `IO`.
-- |
-- | Note that you can use "do notation" directly with the `Task` type -- you
-- | don't have to unwrap it first. Essentially, you only need to unwrap the
-- | `Task` if you need to interact with the `IO` type.
toIO :: ∀ x a. Task x a -> IO (Either x a)
toIO = runExceptT


-- | Given an `IO`, make a `Task`. Basically, just wraps it in an `ExceptT`.
fromIO :: ∀ x a. IO (Either x a) -> Task x a
fromIO = ExceptT


-- | Like `makeAff`, but you get a `Task` back.
makeTask ∷ ∀ e x a. ((Either Error (Either x a) → Eff e Unit) → Eff e (Canceler e)) → Task x a
makeTask =
    -- TODO: Define in terms of IOSync instead of Eff?
    ExceptT <<< liftAff <<< makeAff


newtype EffFnTask e x a = EffFnTask (EffFn3 e (EffFnCb e Error) (EffFnCb e x) (EffFnCb e a) (EffFnCanceler e))

-- | Lifts an FFI function into a `Task`, in the simplest possibly way. This
-- | is like Purescript's `fromEffFnAff`, but allows the Javascript function
-- | to call back with either:
-- |
-- | - A Javascript exception
-- | - The error type of the Task itself
-- | - The success type
-- |
-- | A definition might look like this
-- |
-- | ```javascript
-- | exports.myTaskImpl = function (onException, onError, onSuccess) {
-- |   var cancel = doSomethingAsync(function (err, res) {
-- |     if (err) {
-- |       // This must be the Task's error type
-- |       onError(err);
-- |
-- |       // Or, if err is a Javascript exception that you don't
-- |       // want to handle in the Tasks error type, you can do this
-- |       // instead.
-- |       // onException(err);
-- |     } else {
-- |       // This must be the Task's success type
-- |       onSuccess(res);
-- |     }
-- |   });
-- |   return function (cancelError, onCancelerError, onCancelerSuccess) {
-- |     cancel();
-- |     onCancelerSuccess();
-- |   };
-- | };
-- | ```
-- |
-- | ```purescript
-- | foreign import myTaskImpl :: ∀ eff. EffFnTask (myeffect :: MYEFFECT | eff) Int String
-- |
-- | myTask :: ∀ eff. TaskE (myeffect :: MYEFFECT | eff) Int String
-- | myTask = fromEffFnTask myTaskImpl
-- | ````
fromEffFnTask ∷ ∀ e x a. EffFnTask e x a → Task x a
fromEffFnTask (EffFnTask ffi) =
    makeTask \k → do
        EffFnCanceler canceler ←
            runEffFn3 ffi
                (mkEffFn1 (k <<< Left)) -- the JS exception callback
                (mkEffFn1 (k <<< Right <<< Left)) -- the Task fail
                (mkEffFn1 (k <<< Right <<< Right)) -- the Task succeed
        pure $ Canceler \e → makeAff \k2 → do
            runEffFn3 canceler e
                (mkEffFn1 (k2 <<< Left))
                (mkEffFn1 (k2 <<< Right))
            pure nonCanceler


-- | A task that succeeds immediately when run.
-- |
-- |     succeed 42    -- results in 42
-- |
-- | Equivalent to Purescript's `pure`.
succeed :: ∀ x a. a -> Task x a
succeed = pure


-- | A task that fails immediately when run.
-- |
-- |     fail "file not found" : Task String a
-- |
-- | Equivalent to Purescript's `throwError`.
fail :: ∀ x a. x -> Task x a
fail = throwError


-- ERRORS

-- | Recover from a failure in a task. If the given task fails, we use the
-- | callback to recover.
-- |
-- |     fail "file not found"
-- |       |> onError (\msg -> succeed 42)
-- |       -- succeed 42
-- |
-- |     succeed 9
-- |       |> onError (\msg -> succeed 42)
-- |       -- succeed 9
-- |
-- | Like Purescript's `catchError`, but with a different signature.
-- |
-- | The arguments to this function were flipped in Elm 0.18.
onError :: ∀ x y a. (x -> Task y a) -> Task x a -> Task y a
onError handler task =
    -- This is equivalent to `catchError`, but that doesn't work by itself,
    -- because it would need a signature of
    --
    --   ∀ x a. Task x a -> (x -> Task x a) -> Task x a
    --
    -- That is, `catchError` can't change the error type.
    ExceptT (
        runExceptT task >>=
            either
                (runExceptT <<< handler)
                (pure <<< Right)
    )


-- | Transform the error value. This can be useful if you need a bunch of error
-- | types to match up.
-- |
-- |     type Error = Http Http.Error | WebGL WebGL.Error
-- |
-- |     getResources : Task Error Resource
-- |     getResources =
-- |       sequence [ mapError Http serverTask, mapError WebGL textureTask ]
-- |
-- | Equivalent to Purescript's `withExceptT`.
mapError :: ∀ x y a. (x -> y) -> Task x a -> Task y a
mapError = withExceptT


-- | Translate a task that can fail into a task that can never fail, by
-- | converting any failure into `Nothing` and any success into `Just` something.
-- |
-- |     toMaybe (fail "file not found") -- succeed Nothing
-- |     toMaybe (succeed 42)            -- succeed (Just 42)
-- |
-- | This means you can handle the error with the `Maybe` module instead.
-- |
-- | This function was removed in Elm 0.18.
toMaybe :: ∀ x y a. Task x a -> Task y (Maybe a)
toMaybe task =
    onError (\_ -> succeed Nothing) $
        map Just task


-- | If you are chaining together a bunch of tasks, it may be useful to treat
-- | a maybe value like a task.
-- |
-- |     fromMaybe "file not found" Nothing   -- fail "file not found"
-- |     fromMaybe "file not found" (Just 42) -- succeed 42
-- |
-- | This function was removed in Elm 0.18.
fromMaybe :: ∀ x a. x -> Maybe a -> Task x a
fromMaybe default maybe =
    case maybe of
        Just value -> succeed value
        Nothing -> fail default


-- | Translate a task that can fail into a task that can never fail, by
-- | converting any failure into `Err` something and any success into `Ok` something.
-- |
-- |     toResult (fail "file not found") -- succeed (Err "file not found")
-- |     toResult (succeed 42)            -- succeed (Ok 42)
-- |
-- | This means you can handle the error with the `Result` module instead.
-- |
-- | This function was removed in Elm 0.18.
toResult :: ∀ x y a. Task x a -> Task y (Result x a)
toResult task =
    map Ok task
        |> onError (\msg -> succeed (Err msg))


-- | If you are chaining together a bunch of tasks, it may be useful to treat
-- | a result like a task.
-- |
-- |     fromResult (Err "file not found") -- fail "file not found"
-- |     fromResult (Ok 42)                -- succeed 42
-- |
-- | This function was removed in Elm 0.18.
fromResult :: ∀ x a. Result x a -> Task x a
fromResult result =
    case result of
        Ok value -> succeed value
        Err msg -> fail msg


-- THREADS

-- | Abstract type that uniquely identifies a thread.
-- |
-- | This type was renamed `Id` and moved to the `Process` module in Elm 0.17.
type ThreadID =
    ProcessId



-- COMMANDS


data MyCmd msg =
    Perform (Task Never msg)


instance functorMyCmd :: Functor MyCmd where
    map tagger (Perform task) =
        Perform (map tagger task)


-- | The only way to *do* things in Elm is to give commands to the Elm runtime.
-- | So we describe some complex behavior with a `Task` and then command the runtime
-- | to `perform` that task. For example, getting the current time looks like this:
-- |
-- |     import Task
-- |     import Time exposing (Time)
-- |
-- |     type Msg = Click | NewTime Time
-- |
-- |     update : Msg -> Model -> ( Model, Cmd Msg )
-- |     update msg model =
-- |       case msg of
-- |         Click ->
-- |           ( model, Task.perform NewTime Time.now )
-- |
-- |         NewTime time ->
-- |           ...
perform :: ∀ a msg. Partial => (a -> msg) -> Task Never a -> Cmd msg
perform toMessage task =
    command taskManager $ Perform $ map toMessage task


-- | Command the Elm runtime to attempt a task that might fail!
attempt :: ∀ x a msg. Partial => (Result x a -> msg) -> Task x a -> Cmd msg
attempt resultToMessage task =
    task
        |> andThen (succeed <<< resultToMessage <<< Ok)
        |> onError (succeed <<< resultToMessage <<< Err)
        |> Perform
        |> command taskManager


-- MANAGER

-- We have no subs or msg type ... for the moment, using Proxy and Unit ...
-- possibly better options exist.
taskManager :: ∀ appMsg. Partial => Manager MyCmd Proxy appMsg Unit Unit
taskManager = {init, onEffects, onSelfMsg}


init :: Task Never Unit
init =
    succeed unit


onEffects :: ∀ appMsg. Partial => Platform.Router appMsg Unit -> List (MyCmd appMsg) -> List (Proxy appMsg) -> Unit -> Task Never Unit
onEffects router commands subs state =
    map
        (\_ -> unit)
        (sequence (List.map (spawnCmd router) commands))


onSelfMsg :: ∀ appMsg. Platform.Router appMsg Unit -> Unit -> Unit -> Task Never Unit
onSelfMsg _ _ _ =
    succeed unit


spawnCmd :: ∀ x appMsg. Partial => Platform.Router appMsg Unit -> MyCmd appMsg -> Task x Unit
spawnCmd router (Perform task) =
    const (pure unit) <$> spawn $ task >>= Platform.sendToApp router
