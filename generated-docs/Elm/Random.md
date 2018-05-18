## Module Elm.Random

> This library helps you generate pseudo-random values.
>
> This library is all about building generators for whatever
> type of values you need. There are a bunch of primitive generators like
> [`bool`](#v:bool) and [`int`](#v:int) that you can build up into fancier
> generators with functions like [`list`](#v:list) and [`map`](#v:map).
>
> It may be helpful to
> [read about JSON decoders](https://evancz.gitbooks.io/an-introduction-to-elm/content/interop/json.html)
> because they work very similarly.
>
> > *Note:* This is an implementation of the Portable Combined Generator of
> L'Ecuyer for 32-bit computers. It is almost a direct translation from the
> [System.Random](http://hackage.haskell.org/package/random-1.0.1.1/docs/System-Random.html)
> module. It has a period of roughly 2.30584e18.

This is a translation of the Elm code to Purescript.

#### `andThen`

``` purescript
andThen :: forall a b. Generator a -> (a -> Generator b) -> Generator b
```

> Chain random operations, threading through the seed. In the following
> example, we will generate a random letter by putting together uppercase and
> lowercase letters.
>
>     letter :: Generator Char
>     letter =
>       bool `andThen` \b ->
>         if b then uppercaseLetter else lowercaseLetter
>
>     -- bool :: Generator Bool
>     -- uppercaseLetter :: Generator Char
>     -- lowercaseLetter :: Generator Char

#### `map`

``` purescript
map :: forall a b. (a -> b) -> Generator a -> Generator b
```

> Transform the values produced by a generator. The following examples show
> how to generate booleans and letters based on a basic integer generator.
>
>     bool :: Generator Bool
>     bool =
>       map ((==) 1) (int 0 1)
>
>     lowercaseLetter :: Generator Char
>     lowercaseLetter =
>       map (\n -> Char.fromCode (n + 97)) (int 0 25)
>
>     uppercaseLetter :: Generator Char
>     uppercaseLetter =
>       map (\n -> Char.fromCode (n + 65)) (int 0 25)

#### `Generator`

``` purescript
data Generator a
```

> A `Generator` is like a recipe for generating certain random values. So a
> `Generator Int` describes how to generate integers and a `Generator String`
> describes how to generate strings.
>
> To actually *run* a generator and produce the random values, you need to use
> functions like [`generate`](#v:generate) and [`initialSeed`](#v:initialSeed).

##### Instances
``` purescript
Functor Generator
Apply Generator
Bind Generator
(Semigroup a) => Semigroup (Generator a)
```

#### `Seed`

``` purescript
data Seed
```

> A `Seed` is the source of randomness in this whole system. Whenever
> you want to use a generator, you need to pair it with a seed.

#### `bool`

``` purescript
bool :: Generator Bool
```

> Create a generator that produces boolean values. The following example
> simulates a coin flip that may land heads or tails.
>
>     data Flip = Heads | Tails
>
>     coinFlip :: Generator Flip
>     coinFlip =
>         map (\b -> if b then Heads else Tails) bool

#### `int`

``` purescript
int :: forall a. Ord a => Int53Value a => a -> a -> Generator a
```

> Generate 32-bit integers in a given range.
>
>     int 0 10   -- an integer between zero and ten
>     int (-5) 5   -- an integer between -5 and 5
>
>     int minInt maxInt  -- an integer in the widest range feasible

You can supply either `Int` or `Int53` for the parameters.

#### `float`

``` purescript
float :: Float -> Float -> Generator Float
```

> Generate floats in a given range. The following example is a generator
> that produces decimals between 0 and 1.
>
>     probability :: Generator Float
>     probability =
>         float 0 1

#### `list`

``` purescript
list :: forall t a. Monoid (t a) => Applicative t => Int -> Generator a -> Generator (t a)
```

> Create a list of random values.
>
>     floatList :: Generator (List Float)
>     floatList =
>         list 10 (float 0 1)
>
>     intList :: Generator (List Int)
>     intList =
>         list 5 (int 0 100)
>
>     intPairs :: Generator (List (Tuple Int Int))
>     intPairs =
>         list 10 <| pair (int 0 100) (int 0 100)

The return type is polymorphic in order to accommodate `List` or `Array`, among others.

#### `pair`

``` purescript
pair :: forall a b. Generator a -> Generator b -> Generator (Tuple a b)
```

> Create a pair of random values. A common use of this might be to generate
> a point in a certain 2D space. Imagine we have a collage that is 400 pixels
> wide and 200 pixels tall.
>
>     randomPoint :: Generator (Tuple Int Int)
>     randomPoint =
>         pair (int -200 200) (int -100 100)

#### `minInt`

``` purescript
minInt :: Int53
```

> The minimum value for randomly generated 32-bit ints: -2147483648.

#### `maxInt`

``` purescript
maxInt :: Int53
```

> The maximum value for randomly generated 32-bit ints: 2147483647.

#### `step`

``` purescript
step :: forall a. Generator a -> Seed -> Generated a
```

> Generate a random value as specified by a given `Generator`.
>
> In the following example, we are trying to generate a number between 0 and 100
> with the `int 0 100` generator. Each time we call `step` we need to provide a
> seed. This will produce a random number and a *new* seed to use if we want to
> run other generators later.
>
> So here it is done right, where we get a new seed from each `step` call and
> thread that through.
>
>     seed0 = initialSeed 31415
>
>     -- step (int 0 100) seed0 ==> {value: 42, seed: seed1}
>     -- step (int 0 100) seed1 ==> {value: 31, seed: seed2}
>     -- step (int 0 100) seed2 ==> (value: 99, seed: seed3}
>
> Notice that we use different seeds on each line. This is important! If you use
> the same seed, you get the same results.
>
>     -- step (int 0 100) seed0 ==> {value: 42, seed: seed1}
>     -- step (int 0 100) seed0 ==> {value: 42, seed: seed1}
>     -- step (int 0 100) seed0 ==> {value: 42, seed: seed1}

Prior to Elm 0.17, this function was called `generate`.

#### `initialSeed`

``` purescript
initialSeed :: forall a. Int53Value a => a -> Seed
```

> Create a &ldquo;seed&rdquo; of randomness which makes it possible to
> generate random values. If you use the same seed many times, it will result
> in the same thing every time!

You can supply either an `Int` or `Int53` for the parameter.

#### `generate`

``` purescript
generate :: forall a msg. (a -> msg) -> Generator a -> Cmd msg
```

> Create a command that will generate random values.
>
> Read more about how to use this in your programs in [The Elm Architecture
> tutorial][arch] which has a section specifically [about random values][rand].
>
> [arch]: https://evancz.gitbooks.io/an-introduction-to-elm/content/architecture/index.html
> [rand]: https://evancz.gitbooks.io/an-introduction-to-elm/content/architecture/effects/random.html


### Re-exported from Elm.Apply:

#### `map2`

``` purescript
map2 :: forall w a b c. Apply w => (a -> b -> c) -> w a -> w b -> w c
```

Map a function of two arguments over some container type.

The equivalent of Purescript's `lift2`.

### Re-exported from Elm.Task:

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

#### `map`

``` purescript
map :: forall a b f. Functor f => (a -> b) -> f a -> f b
```

#### `andThen`

``` purescript
andThen :: forall m a b. Bind m => (a -> m b) -> m a -> m b
```

Given some computation, chain its result with another computation.

Equivalent to Purescript's `bind`.

The order of the arguments was flipped in Elm 0.18.

