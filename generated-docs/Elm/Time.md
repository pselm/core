## Module Elm.Time

Library for working with time

#### `Time`

``` purescript
type Time = Number
```

Type alias to make it clearer when you are working with time values.
Using the `Time` helpers like `second` and `inSeconds` instead of raw numbers
is very highly recommended.

Note that Purescript's `Data.Time` class does something similar with its
`Duration` class, which has separate types for `Hours`, `Minutes`, `Seconds`
and `Milliseconds`.

#### `toTime`

``` purescript
toTime :: forall a. Duration a => a -> Time
```

Convert any of Purescript's `Duration` types to `Time`.

#### `fromTime`

``` purescript
fromTime :: forall a. Duration a => Time -> a
```

Convert from `Time` to any of Purescript's `Duration` types.

#### `now`

``` purescript
now :: forall x. Task x Time
```

Get the `Time` at the moment when this task is run.

#### `every`

``` purescript
every :: forall msg. Time -> (Time -> msg) -> Sub msg
```

Subscribe to the current time. First you provide an interval describing how
frequently you want updates. Second, you give a tagger that turns a time into a
message for your `update` function. So if you want to hear about the current
time every second, you would say something like this:

    type Msg = Tick Time | ...

    subscriptions model =
      every second Tick

Check out the [Elm Architecture Tutorial][arch] for more info on how
subscriptions work.

[arch]: https://github.com/evancz/elm-architecture-tutorial/

**Note:** this function is not for animation! You need to use something based
on `requestAnimationFrame` to get smooth animations. This is based on
`setInterval` which is better for recurring tasks like “check on something
every 30 seconds”.

#### `millisecond`

``` purescript
millisecond :: Time
```

Units of time, making it easier to specify things like a half-second
`(500 * millisecond)` without remembering Elm&rsquo;s underlying units of time.

#### `second`

``` purescript
second :: Time
```

#### `minute`

``` purescript
minute :: Time
```

#### `hour`

``` purescript
hour :: Time
```

#### `inMilliseconds`

``` purescript
inMilliseconds :: Time -> Float
```

#### `inSeconds`

``` purescript
inSeconds :: Time -> Float
```

#### `inMinutes`

``` purescript
inMinutes :: Time -> Float
```

#### `inHours`

``` purescript
inHours :: Time -> Float
```


### Re-exported from Data.Time.Duration:

#### `Duration`

``` purescript
class Duration a 
```

A class for enabling conversions between duration types.

##### Instances
``` purescript
Duration Milliseconds
Duration Seconds
Duration Minutes
Duration Hours
Duration Days
```

