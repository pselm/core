## Module Elm.Apply

Elm modules typically use `map2` through `map5` for what Purescript's
`Apply` class would call `lift2` through `lift5`.

So, we define `map2` through `map5` here as synonyms for `lift2` through
`lift5`. We also re-export these in the individual Elm modules that use
them, so that that the API matches with the Elm API.

A few Elm modules use up to `map8`, so we implement those here as well.

We also make `andMap` a synonym for Purescript's `apply`.

#### `andMap`

``` purescript
andMap :: forall a b f. Apply f => f (a -> b) -> f a -> f b
```

Map a function in a container to a value in a container.

This is the equivalent of Purescript's `apply`.

#### `map2`

``` purescript
map2 :: forall w a b c. Apply w => (a -> b -> c) -> w a -> w b -> w c
```

Map a function of two arguments over some container type.

The equivalent of Purescript's `lift2`.

#### `map3`

``` purescript
map3 :: forall w a b c d. Apply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
```

Map a function of three arguments over some container type.

The equivalent of Purescript's `lift3`.

#### `map4`

``` purescript
map4 :: forall w a b c d e. Apply w => (a -> b -> c -> d -> e) -> w a -> w b -> w c -> w d -> w e
```

Map a function of four arguments over some container type.

The equivalent of Purescript's `lift4`.

#### `map5`

``` purescript
map5 :: forall w a b c d e f. Apply w => (a -> b -> c -> d -> e -> f) -> w a -> w b -> w c -> w d -> w e -> w f
```

Map a function of five arguments over some container type.

The equivalent of Purescript's `lift5`.

#### `map6`

``` purescript
map6 :: forall w a b c d e f g. Apply w => (a -> b -> c -> d -> e -> f -> g) -> w a -> w b -> w c -> w d -> w e -> w f -> w g
```

Map a function of six arguments over some container type.

#### `map7`

``` purescript
map7 :: forall w a b c d e f g h. Apply w => (a -> b -> c -> d -> e -> f -> g -> h) -> w a -> w b -> w c -> w d -> w e -> w f -> w g -> w h
```

Map a function of seven arguments over some container type.

#### `map8`

``` purescript
map8 :: forall w a b c d e f g h i. Apply w => (a -> b -> c -> d -> e -> f -> g -> h -> i) -> w a -> w b -> w c -> w d -> w e -> w f -> w g -> w h -> w i
```

Map a function of eight arguments over some container type.


