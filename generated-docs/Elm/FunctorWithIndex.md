## Module Elm.FunctorWithIndex

Elm modules sometimes define `indexedMap` as a synonym for Purescript's
`mapWithIndex`. So, we define that generally here.

#### `indexedMap`

``` purescript
indexedMap :: forall i f a b. FunctorWithIndex i f => (i -> a -> b) -> f a -> f b
```

Map over a container with an index.

Equivalent to Purescript's `mapWithIndex`


