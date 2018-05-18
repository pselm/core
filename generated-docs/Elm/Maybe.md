## Module Elm.Maybe

> This library fills a bunch of important niches in Elm. A `Maybe` can help
> you with optional arguments, error handling, and records with optional fields.

This is implemented in terms of Purescript's `Data.Maybe`, so you can use functions
from there on `Maybe` values as well.

#### `withDefault`

``` purescript
withDefault :: forall a. a -> Maybe a -> a
```

> Provide a default value, turning an optional value into a normal
> value.  This comes in handy when paired with functions like
> [`Dict.get`](Dict#get) which gives back a `Maybe`.
>
>     withDefault 100 (Just 42)   -- 42
>     withDefault 100 Nothing     -- 100
>
>     withDefault "unknown" (Dict.get "Tom" Dict.empty)   -- "unknown"

Equivalent to Purescript's 'fromMaybe`.


### Re-exported from Data.Foldable:

#### `oneOf`

``` purescript
oneOf :: forall f g a. Foldable f => Plus g => f (g a) -> g a
```

Combines a collection of elements using the `Alt` operation.

### Re-exported from Data.Maybe:

#### `Maybe`

``` purescript
data Maybe a
  = Nothing
  | Just a
```

The `Maybe` type is used to represent optional values and can be seen as
something like a type-safe `null`, where `Nothing` is `null` and `Just x`
is the non-null value `x`.

##### Instances
``` purescript
Functor Maybe
Apply Maybe
Applicative Maybe
Alt Maybe
Plus Maybe
Alternative Maybe
Bind Maybe
Monad Maybe
MonadZero Maybe
Extend Maybe
Invariant Maybe
(Semigroup a) => Semigroup (Maybe a)
(Semigroup a) => Monoid (Maybe a)
(Eq a) => Eq (Maybe a)
Eq1 Maybe
(Ord a) => Ord (Maybe a)
Ord1 Maybe
(Bounded a) => Bounded (Maybe a)
(Show a) => Show (Maybe a)
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

### Re-exported from Elm.Bind:

#### `andThen`

``` purescript
andThen :: forall m a b. Bind m => (a -> m b) -> m a -> m b
```

Given some computation, chain its result with another computation.

Equivalent to Purescript's `bind`.

The order of the arguments was flipped in Elm 0.18.

### Re-exported from Prelude:

#### `map`

``` purescript
map :: forall a b f. Functor f => (a -> b) -> f a -> f b
```

