## Module Elm.Task

Tasks make it easy to describe asynchronous operations that may fail,
like HTTP requests or writing to a database.

This is implemented on top of Purescript's `Aff` type. The main difference
is that `Task` has a polymorphically-typed error channel, whereas
the error channel for `Aff` can only represent a `String`.

#### `toIO`

``` purescript
toIO :: forall x a. Task x a -> IO (Either x a)
```

Takes a `Task` and unwraps the underlying `IO`.

Note that you can use "do notation" directly with the `Task` type -- you
don't have to unwrap it first. Essentially, you only need to unwrap the
`Task` if you need to interact with the `IO` type.

#### `fromIO`

``` purescript
fromIO :: forall x a. IO (Either x a) -> Task x a
```

Given an `IO`, make a `Task`. Basically, just wraps it in an `ExceptT`.

#### `makeTask`

``` purescript
makeTask :: forall e x a. ((Either Error (Either x a) -> Eff e Unit) -> Eff e (Canceler e)) -> Task x a
```

Like `makeAff`, but you get a `Task` back.

#### `EffFnTask`

``` purescript
newtype EffFnTask e x a
```

#### `fromEffFnTask`

``` purescript
fromEffFnTask :: forall e x a. EffFnTask e x a -> Task x a
```

Lifts an FFI function into a `Task`, in the simplest possibly way. This
is like Purescript's `fromEffFnAff`, but allows the Javascript function
to call back with either:

- A Javascript exception
- The error type of the Task itself
- The success type

A definition might look like this

```javascript
exports.myTaskImpl = function (onException, onError, onSuccess) {
  var cancel = doSomethingAsync(function (err, res) {
    if (err) {
      // This must be the Task's error type
      onError(err);

      // Or, if err is a Javascript exception that you don't
      // want to handle in the Tasks error type, you can do this
      // instead.
      // onException(err);
    } else {
      // This must be the Task's success type
      onSuccess(res);
    }
  });
  return function (cancelError, onCancelerError, onCancelerSuccess) {
    cancel();
    onCancelerSuccess();
  };
};
```

```purescript
foreign import myTaskImpl :: ∀ eff. EffFnTask (myeffect :: MYEFFECT | eff) Int String

myTask :: ∀ eff. TaskE (myeffect :: MYEFFECT | eff) Int String
myTask = fromEffFnTask myTaskImpl
````

#### `succeed`

``` purescript
succeed :: forall x a. a -> Task x a
```

A task that succeeds immediately when run.

    succeed 42    -- results in 42

Equivalent to Purescript's `pure`.

#### `fail`

``` purescript
fail :: forall x a. x -> Task x a
```

A task that fails immediately when run.

    fail "file not found" : Task String a

Equivalent to Purescript's `throwError`.

#### `mapError`

``` purescript
mapError :: forall x y a. (x -> y) -> Task x a -> Task y a
```

Transform the error value. This can be useful if you need a bunch of error
types to match up.

    type Error = Http Http.Error | WebGL WebGL.Error

    getResources : Task Error Resource
    getResources =
      sequence [ mapError Http serverTask, mapError WebGL textureTask ]

Equivalent to Purescript's `withExceptT`.

#### `onError`

``` purescript
onError :: forall x y a. (x -> Task y a) -> Task x a -> Task y a
```

Recover from a failure in a task. If the given task fails, we use the
callback to recover.

    fail "file not found"
      |> onError (\msg -> succeed 42)
      -- succeed 42

    succeed 9
      |> onError (\msg -> succeed 42)
      -- succeed 9

Like Purescript's `catchError`, but with a different signature.

The arguments to this function were flipped in Elm 0.18.

#### `perform`

``` purescript
perform :: forall a msg. (a -> msg) -> Task Never a -> Cmd msg
```

The only way to *do* things in Elm is to give commands to the Elm runtime.
So we describe some complex behavior with a `Task` and then command the runtime
to `perform` that task. For example, getting the current time looks like this:

    import Task
    import Time exposing (Time)

    type Msg = Click | NewTime Time

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
      case msg of
        Click ->
          ( model, Task.perform NewTime Time.now )

        NewTime time ->
          ...

#### `attempt`

``` purescript
attempt :: forall x a msg. (Result x a -> msg) -> Task x a -> Cmd msg
```

Command the Elm runtime to attempt a task that might fail!

#### `toMaybe`

``` purescript
toMaybe :: forall x y a. Task x a -> Task y (Maybe a)
```

Translate a task that can fail into a task that can never fail, by
converting any failure into `Nothing` and any success into `Just` something.

    toMaybe (fail "file not found") -- succeed Nothing
    toMaybe (succeed 42)            -- succeed (Just 42)

This means you can handle the error with the `Maybe` module instead.

This function was removed in Elm 0.18.

#### `fromMaybe`

``` purescript
fromMaybe :: forall x a. x -> Maybe a -> Task x a
```

If you are chaining together a bunch of tasks, it may be useful to treat
a maybe value like a task.

    fromMaybe "file not found" Nothing   -- fail "file not found"
    fromMaybe "file not found" (Just 42) -- succeed 42

This function was removed in Elm 0.18.

#### `toResult`

``` purescript
toResult :: forall x y a. Task x a -> Task y (Result x a)
```

Translate a task that can fail into a task that can never fail, by
converting any failure into `Err` something and any success into `Ok` something.

    toResult (fail "file not found") -- succeed (Err "file not found")
    toResult (succeed 42)            -- succeed (Ok 42)

This means you can handle the error with the `Result` module instead.

This function was removed in Elm 0.18.

#### `fromResult`

``` purescript
fromResult :: forall x a. Result x a -> Task x a
```

If you are chaining together a bunch of tasks, it may be useful to treat
a result like a task.

    fromResult (Err "file not found") -- fail "file not found"
    fromResult (Ok 42)                -- succeed 42

This function was removed in Elm 0.18.

#### `ThreadID`

``` purescript
type ThreadID = ProcessId
```

Abstract type that uniquely identifies a thread.

This type was renamed `Id` and moved to the `Process` module in Elm 0.17.


### Re-exported from Data.Traversable:

#### `sequence`

``` purescript
sequence :: forall a m t. Traversable t => Applicative m => t (m a) -> m (t a)
```

### Re-exported from Elm.Apply:

#### `map5`

``` purescript
map5 :: forall w a b c d e f. Apply w => (a -> b -> c -> d -> e -> f) -> w a -> w b -> w c -> w d -> w e -> w f
```

Map a function of five arguments over some container type.

The equivalent of Purescript's `lift5`.

#### `map4`

``` purescript
map4 :: forall w a b c d e. Apply w => (a -> b -> c -> d -> e) -> w a -> w b -> w c -> w d -> w e
```

Map a function of four arguments over some container type.

The equivalent of Purescript's `lift4`.

#### `map3`

``` purescript
map3 :: forall w a b c d. Apply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
```

Map a function of three arguments over some container type.

The equivalent of Purescript's `lift3`.

#### `map2`

``` purescript
map2 :: forall w a b c. Apply w => (a -> b -> c) -> w a -> w b -> w c
```

Map a function of two arguments over some container type.

The equivalent of Purescript's `lift2`.

#### `andMap`

``` purescript
andMap :: forall a b f. Apply f => f (a -> b) -> f a -> f b
```

Map a function in a container to a value in a container.

This is the equivalent of Purescript's `apply`.

### Re-exported from Elm.Bind:

#### `andThen`

``` purescript
andThen :: forall m a b. Bind m => (a -> m b) -> m a -> m b
```

Given some computation, chain its result with another computation.

Equivalent to Purescript's `bind`.

The order of the arguments was flipped in Elm 0.18.

### Re-exported from Elm.Platform:

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

### Re-exported from Prelude:

#### `map`

``` purescript
map :: forall a b f. Functor f => (a -> b) -> f a -> f b
```

