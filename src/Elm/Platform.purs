
module Elm.Platform
  ( Program, program, programWithFlags
  , runProgram
  , Task, ProcessId
  , Router, sendToApp, sendToSelf
  , Manager
  , Cmd, command
  , Sub, subscription
  ) where


import Control.Monad.Aff (Fiber)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.IO (INFINITY, IO)
import Data.List (List)
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple)
import Elm.Basics (Never)
import Partial (crash)
import Prelude (class Functor, class Semigroup, Unit, append, const, map, pure, ($))
import Unsafe.Coerce (unsafeCoerce)


-- | A `Program` describes how to manage your Elm app.
-- |
-- | You can create [headless][] programs with the [`program`](#program) and
-- | [`programWithFlags`](#programWithFlags) functions. Similar functions exist in
-- | [`Html`][html] that let you specify a view.
-- |
-- | [headless]: https://en.wikipedia.org/wiki/Headless_software
-- | [html]: http://package.elm-lang.org/packages/elm-lang/html/latest/Html
-- |
-- | Honestly, it is totally normal if this seems crazy at first. The best way to
-- | understand is to work through [guide.elm-lang.org](http://guide.elm-lang.org/).
-- | It makes way more sense in context!
newtype Program flags model msg = Program
    -- To start with, let's just record what we get provided
    { init :: flags -> Tuple model (Cmd msg)
    , update :: msg -> model -> Tuple model (Cmd msg)
    , subscriptions :: model -> Sub msg
    }


-- | Create a [headless][] program. This is great if you want to use Elm as the
-- | &ldquo;brain&rdquo; for something else. You can still communicate with JS via
-- | ports and manage your model, you just do not have to specify a `view`.
-- |
-- | [headless]: https://en.wikipedia.org/wiki/Headless_software
-- |
-- | Initializing a headless program from JavaScript looks like this:
-- |
-- | ```javascript
-- | var app = Elm.MyThing.worker();
-- | ```
program :: ∀ model msg.
    { init :: Tuple model (Cmd msg)
    , update :: msg -> model -> Tuple model (Cmd msg)
    , subscriptions :: model -> Sub msg
    }
    -> Program Never model msg
program config =
    programWithFlags $
        config { init = const config.init }


-- | Same as [`program`](#program), but you can provide flags. Initializing a
-- | headless program (with flags) from JavaScript looks like this:
-- |
-- | ```javascript
-- | var app = Elm.MyThing.worker({ user: 'Tom', token: 1234 });
-- | ```
-- |
-- | Whatever argument you provide to `worker` will get converted to an Elm value,
-- | allowing you to configure your Elm program however you want from JavaScript!
programWithFlags :: ∀ flags model msg.
    { init :: flags -> Tuple model (Cmd msg)
    , update :: msg -> model -> Tuple model (Cmd msg)
    , subscriptions :: model -> Sub msg
    }
    -> Program flags model msg
programWithFlags =
    Program


-- | In Elm, you simply assign your `Program` to `main` and it gets run. Here,
-- | we need to explicilty `run` a program to turn it into an `IO` that you can
-- | execute.
-- |
-- | Eventually, this will need to take into account flags and ports, which are
-- | Elm's way of communicating with Javascript. Flags would be an argument
-- | that the Javascript side can call when starting the program. Ports would
-- | be something returned by the function that the Javascript side calls,
-- | which the Javascript side can use to subscribe to values and send values.
-- |
-- | So, eventually this will problem look more like:
-- |
-- |     runProgram :: ∀ flags model msg. Foreign -> Program flags model msg -> IO Ports
-- |
-- | for some definition of `Ports` (in fact, there's an extra level of
-- | indirection in Elm with the `.worker()` call, so we could set that up as
-- | well). And, `flags` will probably need a typeclass constraint in order to
-- | support the auto-decoding that Elm does. (We can probably auto-decode
-- | using generics).
runProgram :: ∀ flags model msg. Partial => Program flags model msg -> IO Unit
runProgram =
    crash


-- TASKS and PROCESSES

-- | Represents asynchronous effects that may fail. It is useful for stuff like
-- | HTTP.
-- |
-- | For example, maybe we have a task with the type (`Task String User`). This means
-- | that when we perform the task, it will either fail with a `String` message or
-- | succeed with a `User`. So this could represent a task that is asking a server
-- | for a certain user.
-- |
-- | Implemented in terms of Purescript's `IO` type, with `ExceptT` layered on top
-- | in order to provide for a polymorphically-typed error channel.
-- |
-- | We use `IO` rather than `Aff` because the effects-tracking complicates the
-- | conversion of Elm code, and Purescript is moving away from it anyway.
type Task x a =
    ExceptT x IO a


-- | Head over to the documentation for the [`Process`](Process) module for
-- | information on this. It is only defined here because it is a platform
-- | primitive.
type ProcessId =
    Fiber (infinity :: INFINITY) Unit


-- EFFECT MANAGER INTERNALS

-- There is a nice disucssion of effects modules here:
--
-- http://simonh1000.github.io/2017/05/effect-managers/
--
-- Note that I'm not sure yet exactly what `Cmd` and `Sub` will be, or, for
-- that matter, exactly what the `command` function is. Part of what's likely
-- to be interesting here is the combination of multiple effects managers,
-- which Elm can do "behind the scenes" (and, I suppose, we could as well if
-- we must), but which would be interesting to try to write types for.
--
-- Also, one of my goals is to allow Elm programs to be converted with as
-- little fuss as possible, including effects modules. So, ideally I'd like to
-- work as much as possible with the type signaturs that you find in effects
-- modules. But, one will no doubt need to make some modifications, to handle
-- explicitly those things which Elm does magically.


-- | An effect manager has access to a “router” that routes messages between
-- | the main app and your individual effect manager.
data Router appMsg selfMsg =
    -- Not yet clear what this will actually be, but this is the surface API
    -- from Elm that we want to preserve.
    Router


-- | Send the router a message for the main loop of your app. This message will
-- | be handled by the overall `update` function, just like events from `Html`.
sendToApp :: ∀ x a msg. Partial => Router msg a -> msg -> Task x Unit
sendToApp router msg =
    crash


-- | Send the router a message for your effect manager. This message will
-- | be routed to the `onSelfMsg` function, where you can update the state of your
-- | effect manager as necessary.
-- |
-- | As an example, the effect manager for web sockets
sendToSelf :: ∀ x a msg. Partial => Router a msg -> msg -> Task x Unit
sendToSelf router msg =
    crash


-- | This is an initial attempt to express the things which effects modules
-- | have in common.
-- |
-- | If, in Elm, you see something like:
-- |
-- |     effect module WebSocket where { command = MyCmd, subscription = MySub }
-- |
-- | then the thesis is that the module is defining a (hidden) type which:
-- |
-- |   - can manage commands of the `command` type
-- |   - can manage subsriptions of the `subscription` type
-- |   - keeps some internal state without requiring the user of the module
-- |     to maintain a bunch of boilerplate
-- |   - can handle internal messages without requiring the user of the module
-- |     to maintain a bunch of boilerplate
-- |
-- | So, let's try defining that class here and see how far it gets us.
-- |
-- | We'll start by defining a record type ... this may end up wanting to be a
-- | type-class, of course, but we won't necessarily start with that.
type Manager cmd sub selfMsg state =
    -- | Produces the initial state of the manager.
    { init :: ∀ appMsg. Task Never (state appMsg)

    -- | What we get called with when a round of effects is to be handled.
    , onEffects :: ∀ appMsg. Router appMsg selfMsg -> List (cmd appMsg) -> List (sub appMsg) -> state appMsg -> Task Never (state appMsg)

    -- | Handle our internal messages when we get them ...
    , onSelfMsg :: ∀ appMsg. Router appMsg selfMsg -> selfMsg -> state appMsg -> Task Never (state appMsg)
    }


-- | A command is a way of telling Elm, “Hey, I want you to do this thing!”
-- | So if you want to send an HTTP request, you would need to command Elm to do it.
-- | Or if you wanted to ask for geolocation, you would need to command Elm to go
-- | get it.
-- |
-- | Every `Cmd` specifies (1) which effects you need access to and (2) the type of
-- | messages that will come back into your application.
-- |
-- | **Note:** Do not worry if this seems confusing at first! As with every Elm user
-- | ever, commands will make more sense as you work through [the Elm Architecture
-- | Tutorial](http://guide.elm-lang.org/architecture/index.html) and see how they
-- | fit into a real application!
newtype Cmd msg =
    Cmd (List (ExistsCmd msg))


-- The compiler  could probably derive all of these.
instance functorCmd :: Functor Cmd where
    map func (Cmd list) =
        Cmd $ map (map func) list

instance semigroupCmd :: Semigroup (Cmd msg) where
    append (Cmd list1) (Cmd list2) =
        Cmd $ append list1 list2

instance monoidCmd :: Monoid (Cmd msg) where
    mempty =
        Cmd mempty


-- A `Cmd` is basically a list of existential commands which share the same
-- appMsg, but may differ in other ways.  The internal list gets us an easy
-- implementation for `batch` and `none`.


-- At least initially, our strategy is to use `command` and `subscription`,
-- which the Elm code calls anyway, to bundle up the effects manager with the
-- `Cmd` or the `Sub`. That way, we don't have to pre-collect the effects
-- managers ... we can just use the ones we are given.
--
-- Now, we're ultimately going to need to create some kind of mapping from an
-- effects manager to the state which it wants. Which, I suppose, will require
-- that `command` and `subscription` require an `Ord` instance on `Manager`?
-- Which I suppose can be arranged in one way or another ... possibly via
-- `Type.Typeable` or the equivalent.
--
-- In any event, We're going to need to forget most of these types, except for
-- `appMsg`, in order to work with Elm's `Cmd msg`. So, we have to bundle
-- together everything else we're going to need, so we can work existentially.
type CmdManager cmd sub appMsg selfMsg state =
    { cmd :: cmd appMsg
    , manager :: Manager cmd sub selfMsg state
    , map :: ∀ a b. (a -> b) -> cmd a -> cmd b
    }


-- This is essentially a version of `CmdManager` with some types forgotten. So,
-- it's the `Data.Exists` pattern, but we want to eliminate more than one type
-- variable, so this is handy.
foreign import data ExistsCmd :: Type -> Type

mkExistsCmd :: ∀ appMsg selfMsg cmd sub state. CmdManager cmd sub appMsg selfMsg state -> ExistsCmd appMsg
mkExistsCmd = unsafeCoerce

runExistsCmd :: ∀ appMsg r. (∀ selfMsg cmd sub state. CmdManager cmd sub appMsg selfMsg state -> r) -> ExistsCmd appMsg -> r
runExistsCmd = unsafeCoerce

instance functorExistsCmd :: Functor ExistsCmd where
    map func =
        runExistsCmd \cmdManager ->
            mkExistsCmd $ cmdManager { cmd = cmdManager.map func cmdManager.cmd }


-- | `command` is a magical function in Elm. It gets called by effects modules,
-- | but it isn't explicitly implemented anywhere (probably because it can't be
-- | well-typed in Elm).
-- |
-- | Effects modules implement "commands" as a kind of data structure to
-- | interpret later, which, if one were starting from scratch, you might
-- | implement in Purescript via `Free` (and that may be how it ought to be
-- | done in the end, but I'm working my way into it, by sketching the surface
-- | of the Elm API and then seeing what's needed to implement it).
-- |
-- | The Elm implementation has a couple of interesting characterstics:
-- |
-- | - It is an "open" structure ... that is, the compiler does something to
-- |   bring together and dispatch the commands defined by whatever effects
-- |   modules get used.
-- |
-- | - The Elm API is such that we need to be able to batch commands from
-- |   various effects modules together, in the `Cmd` type, so long as they
-- |   are parameterized by the same `appMsg`. So, that implies, at the very
-- |   least, that there will need to be an existential element to this ...
-- |   we'll need to "forget" some of our type information and yet able to
-- |   "recover" it.
-- |
-- | Now, the special role played by the `command` function (in Elm) is that it
-- | takes whatever type the particular effects module uses for its commands,
-- | and returns a generic `Cmd a` that can be batched with other commands from
-- | other effects modules. So, it is basicaly our "hook" where we can
-- | implement whatever magic is required to make that work.
-- |
-- | At a miminum, we're going to need the effects module to provide its
-- | `Manager`.  Ultimatley, it may be nice for formulate this in terms of type
-- | classes, but I think it will be easier to start by explicitly passing
-- | records around.
command :: ∀ cmd sub appMsg selfMsg state.
    Functor cmd =>
    Manager cmd sub selfMsg state ->
    cmd appMsg ->
    Cmd appMsg
command manager cmd =
    Cmd $ pure $ mkExistsCmd { manager, cmd, map }


-- | A subscription is a way of telling Elm, “Hey, let me know if anything
-- | interesting happens over there!” So if you want to listen for messages on a web
-- | socket, you would tell Elm to create a subscription. If you want to get clock
-- | ticks, you would tell Elm to subscribe to that. The cool thing here is that
-- | this means *Elm* manages all the details of subscriptions instead of *you*.
-- | So if a web socket goes down, *you* do not need to manually reconnect with an
-- | exponential backoff strategy, *Elm* does this all for you behind the scenes!
-- |
-- | Every `Sub` specifies (1) which effects you need access to and (2) the type of
-- | messages that will come back into your application.
-- |
-- | **Note:** Do not worry if this seems confusing at first! As with every Elm user
-- | ever, subscriptions will make more sense as you work through [the Elm Architecture
-- | Tutorial](http://guide.elm-lang.org/architecture/index.html) and see how they fit
-- | into a real application!
newtype Sub msg =
    Sub (List (ExistsSub msg))


-- The instances could probably be derived by the compiler.
instance functorSub :: Functor Sub where
    map func (Sub list) =
        Sub $ map (map func) list

instance semigroupSub :: Semigroup (Sub msg) where
    append (Sub list1) (Sub list2) =
        Sub $ append list1 list2

instance monoidSub :: Monoid (Sub msg) where
    mempty =
        Sub mempty


type SubManager cmd sub appMsg selfMsg state =
    { sub :: sub appMsg
    , manager :: Manager cmd sub selfMsg state
    , map :: ∀ a b. (a -> b) -> sub a -> sub b
    }


foreign import data ExistsSub :: Type -> Type

mkExistsSub :: ∀ appMsg selfMsg cmd sub state. SubManager cmd sub appMsg selfMsg state -> ExistsSub appMsg
mkExistsSub = unsafeCoerce

runExistsSub :: ∀ appMsg r. (∀ selfMsg cmd sub state. SubManager cmd sub appMsg selfMsg state -> r) -> ExistsSub appMsg -> r
runExistsSub = unsafeCoerce

instance functorExistsSub :: Functor ExistsSub where
    map func =
        runExistsSub \subManager ->
            mkExistsSub $ subManager { sub = subManager.map func subManager.sub }


-- | Like `command`, but for subscriptions.
subscription :: ∀ cmd sub appMsg selfMsg state.
    Functor sub =>
    Manager cmd sub selfMsg state ->
    sub appMsg ->
    Sub appMsg
subscription manager sub =
    Sub $ pure $ mkExistsSub { manager, sub, map }
