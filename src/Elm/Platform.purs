
module Elm.Platform
  ( Program, program, programWithFlags
  , Task, TaskE, ProcessId
  -- , Router, sendToApp, sendToSelf
  ) where


import Control.Monad.Aff (Aff)
import Control.Monad.Except.Trans (ExceptT)
import Data.Tuple (Tuple)
import Elm.Basics (Never)
import Elm.Platform.Cmd (Cmd)
import Elm.Platform.Sub (Sub)


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
    { init :: Tuple model (Cmd msg)
    , update :: msg -> model -> Tuple model (Cmd msg)
    , subscriptions :: model -> Sub msg
    }
  -> Program Never model msg
program config =
    Program


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
    { init :: flags -> Tuple model (Cmd msg)
    , update :: msg -> model -> Tuple model (Cmd msg)
    , subscriptions :: model -> Sub msg
    }
  -> Program flags model msg
programWithFlags config =
    Program



-- TASKS and PROCESSES

-- | Represents asynchronous effects that may fail. It is useful for stuff like
-- | HTTP.
-- |
-- | For example, maybe we have a task with the type (`Task String User`). This means
-- | that when we perform the task, it will either fail with a `String` message or
-- | succeed with a `User`. So this could represent a task that is asking a server
-- | for a certain user.
-- |
-- | Implemented in terms of Purescript's `Aff` type, with `ExceptT` layered on top
-- | in order to provide for a polymorphically-typed error channel.
type Task x a =
    ∀ e. ExceptT x (Aff e) a


-- | Equivalent to a `Task`, but with the effect types specified.
type TaskE e x a =
    ExceptT x (Aff e) a


-- | Head over to the documentation for the [`Process`](Process) module for
-- | information on this. It is only defined here because it is a platform
-- | primitive.
data ProcessId =
    ProcessId



-- EFFECT MANAGER INTERNALS


{-| An effect manager has access to a “router” that routes messages between
the main app and your individual effect manager.
-}
{-
type Router appMsg selfMsg =
  Router
-}

{-| Send the router a message for the main loop of your app. This message will
be handled by the overall `update` function, just like events from `Html`.
-}
{-
sendToApp : Router msg a -> msg -> Task x ()
sendToApp =
  Native.Platform.sendToApp
-}

{-| Send the router a message for your effect manager. This message will
be routed to the `onSelfMsg` function, where you can update the state of your
effect manager as necessary.

As an example, the effect manager for web sockets
-}
{-
sendToSelf : Router a msg -> msg -> Task x ()
sendToSelf =
  Native.Platform.sendToSelf
  -}
