## Module Elm.Monoid

Elm modules sometimes define a `none` function that corresponds to Purescript's
`mempty`, and a `batch` function that corresponds to Purescript's `fold`. So,
we re-export these here with the Elm-ish names.

#### `batch`

``` purescript
batch :: forall f m. Foldable f => Monoid m => f m -> m
```

Smush together a bunch of things.

Equivalent to Purescript's `fold`

#### `none`

``` purescript
none :: forall m. Monoid m => m
```

Produce an "empty" value of the relevant type.

Equivalent to Purescript's `mempty`.


