
module Elm.Platform
  ( Program, program, programWithFlags
  , Task, ProcessId
  , Router, sendToApp, sendToSelf
  ) where


import Control.Monad.Aff (Fiber)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.IO (INFINITY, IO)
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


-- | An effect manager has access to a “router” that routes messages between
-- | the main app and your individual effect manager.
data Router appMsg selfMsg =
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
