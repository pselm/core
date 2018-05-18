## Module Elm.Platform

#### `Program`

``` purescript
newtype Program flags model msg
```

> A `Program` describes how to manage your Elm app.
>
> You can create [headless][] programs with the [`program`](#program) and
> [`programWithFlags`](#programWithFlags) functions. Similar functions exist in
> [`Html`][html] that let you specify a view.
>
> [headless]: https://en.wikipedia.org/wiki/Headless_software
> [html]: http://package.elm-lang.org/packages/elm-lang/html/latest/Html
>
> Honestly, it is totally normal if this seems crazy at first. The best way to
> understand is to work through [guide.elm-lang.org](http://guide.elm-lang.org/).
> It makes way more sense in context!

##### Instances
``` purescript
Newtype (Program flags model msg) _
```

#### `program`

``` purescript
program :: forall flags model msg. { init :: Tuple model (Cmd msg), update :: msg -> model -> Tuple model (Cmd msg), subscriptions :: model -> Sub msg } -> Program flags model msg
```

> Create a [headless][] program. This is great if you want to use Elm as the
> &ldquo;brain&rdquo; for something else. You can still communicate with JS via
> ports and manage your model, you just do not have to specify a `view`.
>
> [headless]: https://en.wikipedia.org/wiki/Headless_software
>
> Initializing a headless program from JavaScript looks like this:
>
> ```javascript
> var app = Elm.MyThing.worker();
> ```

#### `programWithFlags`

``` purescript
programWithFlags :: forall flags model msg. { init :: flags -> Tuple model (Cmd msg), update :: msg -> model -> Tuple model (Cmd msg), subscriptions :: model -> Sub msg } -> Program flags model msg
```

> Same as [`program`](#program), but you can provide flags. Initializing a
> headless program (with flags) from JavaScript looks like this:
>
> ```javascript
> var app = Elm.MyThing.worker({ user: 'Tom', token: 1234 });
> ```
>
> Whatever argument you provide to `worker` will get converted to an Elm value,
> allowing you to configure your Elm program however you want from JavaScript!

#### `runProgram`

``` purescript
runProgram :: forall flags model msg. flags -> Program flags model msg -> IO Unit
```

In Elm, you simply assign your `Program` to `main` and it gets run. Here,
we need to explicilty `run` a program to turn it into an `IO` that you can
execute.

Eventually, this will need to take into account flags and ports, which are
Elm's way of communicating with Javascript. Flags would be an argument
that the Javascript side can call when starting the program. Ports would
be something returned by the function that the Javascript side calls,
which the Javascript side can use to subscribe to values and send values.

So, eventually this will problem look more like:

    runProgram :: ∀ flags model msg. Foreign -> Program flags model msg -> IO Ports

for some definition of `Ports` (in fact, there's an extra level of
indirection in Elm with the `.worker()` call, so we could set that up as
well). And, `flags` will probably need a typeclass constraint in order to
support the auto-decoding that Elm does. (We can probably auto-decode
using generics).

Also, eventually this will need to take the `view` into account! (That
code is mostly in elm-lang/html, though some code will be needed here to
integrate with that).

Actually, I think this function will be the "internal" one, so we'll just
require The flags to be prvoided. We'll have another function that is
meant for consumption from Javascript, in which we require that the
`flags` have a `Decode` instance, which you can generate via
purescript-foreign-generics.

#### `Task`

``` purescript
type Task x a = ExceptT x IO a
```

> Represents asynchronous effects that may fail. It is useful for stuff like
> HTTP.
>
> For example, maybe we have a task with the type (`Task String User`). This means
> that when we perform the task, it will either fail with a `String` message or
> succeed with a `User`. So this could represent a task that is asking a server
> for a certain user.

Implemented in terms of Purescript's `IO` type, with `ExceptT` layered on top
in order to provide for a polymorphically-typed error channel.

We use `IO` rather than `Aff` because the effects-tracking complicates the
conversion of Elm code, and Purescript is moving away from it anyway.

#### `ProcessId`

``` purescript
type ProcessId = Fiber (infinity :: INFINITY) Unit
```

> Head over to the documentation for the [`Process`](Process) module for
> information on this. It is only defined here because it is a platform
> primitive.

#### `Router`

``` purescript
data Router appMsg selfMsg
```

> An effect manager has access to a “router” that routes messages between
> the main app and your individual effect manager.

#### `sendToApp`

``` purescript
sendToApp :: forall x a msg. Router msg a -> msg -> Task x Unit
```

> Send the router a message for the main loop of your app. This message will
> be handled by the overall `update` function, just like events from `Html`.

#### `sendToSelf`

``` purescript
sendToSelf :: forall x a msg. Router a msg -> msg -> Task x Unit
```

> Send the router a message for your effect manager. This message will
> be routed to the `onSelfMsg` function, where you can update the state of your
> effect manager as necessary.
>
> As an example, the effect manager for web sockets

#### `Manager`

``` purescript
type Manager cmd sub selfMsg state = { init :: forall appMsg. Task Never (state appMsg), onEffects :: forall appMsg. Router appMsg selfMsg -> List (cmd appMsg) -> List (sub appMsg) -> state appMsg -> Task Never (state appMsg), onSelfMsg :: forall appMsg. Router appMsg selfMsg -> selfMsg -> state appMsg -> Task Never (state appMsg), tag :: String }
```

This is an initial attempt to express the things which effects modules
have in common.

If, in Elm, you see something like:

    effect module WebSocket where { command = MyCmd, subscription = MySub }

then the thesis is that the module is defining a (hidden) type which:

  - can manage commands of the `command` type
  - can manage subsriptions of the `subscription` type
  - keeps some internal state without requiring the user of the module
    to maintain a bunch of boilerplate
  - can handle internal messages without requiring the user of the module
    to maintain a bunch of boilerplate

So, let's try defining that class here and see how far it gets us.

We'll start by defining a record type ... this may end up wanting to be a
type-class, of course, but we won't necessarily start with that.

#### `Cmd`

``` purescript
newtype Cmd msg
```

> A command is a way of telling Elm, “Hey, I want you to do this thing!”
> So if you want to send an HTTP request, you would need to command Elm to do it.
> Or if you wanted to ask for geolocation, you would need to command Elm to go
> get it.
>
> Every `Cmd` specifies (1) which effects you need access to and (2) the type of
> messages that will come back into your application.
>
> **Note:** Do not worry if this seems confusing at first! As with every Elm user
> ever, commands will make more sense as you work through [the Elm Architecture
> Tutorial](http://guide.elm-lang.org/architecture/index.html) and see how they
> fit into a real application!

##### Instances
``` purescript
Functor Cmd
Semigroup (Cmd msg)
Monoid (Cmd msg)
```

#### `command`

``` purescript
command :: forall cmd sub appMsg selfMsg state. Functor cmd => Manager cmd sub selfMsg state -> cmd appMsg -> Cmd appMsg
```

`command` is a magical function in Elm. It gets called by effects modules,
but it isn't explicitly implemented anywhere (probably because it can't be
well-typed in Elm).

Effects modules implement "commands" as a kind of data structure to
interpret later, which, if one were starting from scratch, you might
implement in Purescript via `Free` (and that may be how it ought to be
done in the end, but I'm working my way into it, by sketching the surface
of the Elm API and then seeing what's needed to implement it).

The Elm implementation has a couple of interesting characterstics:

- It is an "open" structure ... that is, the compiler does something to
  bring together and dispatch the commands defined by whatever effects
  modules get used.

- The Elm API is such that we need to be able to batch commands from
  various effects modules together, in the `Cmd` type, so long as they
  are parameterized by the same `appMsg`. So, that implies, at the very
  least, that there will need to be an existential element to this ...
  we'll need to "forget" some of our type information and yet able to
  "recover" it.

Now, the special role played by the `command` function (in Elm) is that it
takes whatever type the particular effects module uses for its commands,
and returns a generic `Cmd a` that can be batched with other commands from
other effects modules. So, it is basicaly our "hook" where we can
implement whatever magic is required to make that work.

At a miminum, we're going to need the effects module to provide its
`Manager`.  Ultimatley, it may be nice for formulate this in terms of type
classes, but I think it will be easier to start by explicitly passing
records around.

#### `Sub`

``` purescript
newtype Sub msg
```

> A subscription is a way of telling Elm, “Hey, let me know if anything
> interesting happens over there!” So if you want to listen for messages on a web
> socket, you would tell Elm to create a subscription. If you want to get clock
> ticks, you would tell Elm to subscribe to that. The cool thing here is that
> this means *Elm* manages all the details of subscriptions instead of *you*.
> So if a web socket goes down, *you* do not need to manually reconnect with an
> exponential backoff strategy, *Elm* does this all for you behind the scenes!
>
> Every `Sub` specifies (1) which effects you need access to and (2) the type of
> messages that will come back into your application.
>
> **Note:** Do not worry if this seems confusing at first! As with every Elm user
> ever, subscriptions will make more sense as you work through [the Elm Architecture
> Tutorial](http://guide.elm-lang.org/architecture/index.html) and see how they fit
> into a real application!

##### Instances
``` purescript
Functor Sub
Semigroup (Sub msg)
Monoid (Sub msg)
```

#### `subscription`

``` purescript
subscription :: forall cmd sub appMsg selfMsg state. Functor sub => Manager cmd sub selfMsg state -> sub appMsg -> Sub appMsg
```

Like `command`, but for subscriptions.


