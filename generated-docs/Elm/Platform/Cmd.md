## Module Elm.Platform.Cmd

# Effects

Elm has **managed effects**, meaning that things like HTTP requests or writing
to disk are all treated as *data* in Elm. When this data is given to the Elm
runtime system, it can do some “query optimization” before actually performing
the effect. Perhaps unexpectedly, this managed effects idea is the heart of why
Elm is so nice for testing, reuse, reproducibility, etc.

There are two kinds of managed effects you will use in your programs: commands
and subscriptions.

#### `withCmds`

``` purescript
withCmds :: forall f model cmd. Foldable f => Monoid cmd => model -> f cmd -> Tuple model cmd
```

#### `(!)`

``` purescript
infixl 5 withCmds as !
```


### Re-exported from Elm.Monoid:

#### `none`

``` purescript
none :: forall m. Monoid m => m
```

Produce an "empty" value of the relevant type.

Equivalent to Purescript's `mempty`.

#### `batch`

``` purescript
batch :: forall f m. Foldable f => Monoid m => f m -> m
```

Smush together a bunch of things.

Equivalent to Purescript's `fold`

### Re-exported from Elm.Platform:

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

### Re-exported from Prelude:

#### `map`

``` purescript
map :: forall a b f. Functor f => (a -> b) -> f a -> f b
```

