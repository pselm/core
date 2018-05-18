## Module Elm.Tuple

Some helpers for working with 2-tuples.

**Note:** For larger chunks of data, it is best to switch to using records. So
instead of representing a 3D point as `(3,4,5)` and wondering why there are no
helper functions, represent it as `{ x = 3, y = 4, z = 5 }` and use all the
built-in syntax for records.

This module was added in Elm 0.18.

Implemented using Purescript's `Data.Tuple`

#### `first`

``` purescript
first :: forall a1 a2. Tuple a1 a2 -> a1
```

Extract the first value from a tuple.

    first (Tuple 3 4) == 3
    first (Tuple "john" "doe") == "john"

Equivalent to Purescript's `fst`

#### `second`

``` purescript
second :: forall a1 a2. Tuple a1 a2 -> a2
```

Extract the second value from a tuple.

   second (Tuple 3 4) == 4
   second (Tuple "john" "doe") == "doe"

Equivalent to Purescript's `snd`

#### `mapFirst`

``` purescript
mapFirst :: forall a b a2. (a -> b) -> Tuple a a2 -> Tuple b a2
```

Transform the first value in a tuple.

    import String

    mapFirst String.reverse (Tuple "stressed" 16) == (Tuple "desserts" 16)
    mapFirst String.length  (Tuple "stressed" 16) == (Tuple 8 16)

Equivalent to Purescript's `lmap`

#### `mapSecond`

``` purescript
mapSecond :: forall a b a1. (a -> b) -> Tuple a1 a -> Tuple a1 b
```

Transform the second value in a tuple.

    import String

    mapSecond sqrt          ("stressed", 16) == ("stressed", 4)
    mapSecond (\x -> x + 1) ("stressed", 16) == ("stressed", 17)

Equivalent to Purescript's `map`


