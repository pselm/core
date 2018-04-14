
module Elm.Platform
  ( Program, program, programWithFlags
  , Task, ProcessId
  , Router, sendToApp, sendToSelf
  , Manager
  ) where


import Control.Monad.Aff (Fiber)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.IO (INFINITY, IO)
import Data.List (List)
import Data.Tuple (Tuple)
import Elm.Basics (Never)
import Elm.Platform.Cmd (Cmd)
import Elm.Platform.Sub (Sub)
import Partial (crash)
import Prelude (Unit)


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
data Program flags model msg =
    Program


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
program ::
    ∀ model msg.
    Partial
    => { init :: Tuple model (Cmd msg)
       , update :: msg -> model -> Tuple model (Cmd msg)
       , subscriptions :: model -> Sub msg
       }
    -> Program Never model msg
program config =
    crash


-- | Same as [`program`](#program), but you can provide flags. Initializing a
-- | headless program (with flags) from JavaScript looks like this:
-- |
-- | ```javascript
-- | var app = Elm.MyThing.worker({ user: 'Tom', token: 1234 });
-- | ```
-- |
-- | Whatever argument you provide to `worker` will get converted to an Elm value,
-- | allowing you to configure your Elm program however you want from JavaScript!
programWithFlags ::
    ∀ flags model msg.
    Partial
    => { init :: flags -> Tuple model (Cmd msg)
       , update :: msg -> model -> Tuple model (Cmd msg)
       , subscriptions :: model -> Sub msg
       }
    -> Program flags model msg
programWithFlags config =
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
type Manager cmd sub appMsg selfMsg state =
    -- | Produces the initial state of the manager.
    { init :: Task Never state

    -- | What we get called with when a round of effects is to be handled.
    , onEffects :: Router appMsg selfMsg -> List (cmd appMsg) -> List (sub appMsg) -> state -> Task Never state

    -- | Handle our internal messages when we get them ...
    , onSelfMsg :: Router appMsg selfMsg -> selfMsg -> state -> Task Never state
    }
