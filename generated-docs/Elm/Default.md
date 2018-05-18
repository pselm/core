## Module Elm.Default

This module re-exports the things which Elm imports by default.

So, if you want the Elm default imports, you can do

`import Elm.Default`


### Re-exported from Data.List:

#### `List`

``` purescript
data List a
```

##### Instances
``` purescript
(Show a) => Show (List a)
(Eq a) => Eq (List a)
Eq1 List
(Ord a) => Ord (List a)
Ord1 List
Semigroup (List a)
Monoid (List a)
Functor List
FunctorWithIndex Int List
Foldable List
FoldableWithIndex Int List
Unfoldable List
Traversable List
TraversableWithIndex Int List
Apply List
Applicative List
Bind List
Monad List
Alt List
Plus List
Alternative List
MonadZero List
MonadPlus List
Extend List
```

#### `(:)`

``` purescript
infixr 6 Cons as :
```

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

### Re-exported from Data.Tuple.Nested:

#### `(/\)`

``` purescript
infixr 6 Tuple as /\
```

Shorthand for constructing n-tuples as nested pairs.
`a /\ b /\ c /\ d /\ unit` becomes `Tuple a (Tuple b (Tuple c (Tuple d unit)))`

#### `type (/\)`

``` purescript
infixr 6 type Tuple as ype (/\
```

Shorthand for constructing n-tuple types as nested pairs.
`forall a b c d. a /\ b /\ c /\ d /\ Unit` becomes
`forall a b c d. Tuple a (Tuple b (Tuple c (Tuple d Unit)))`

### Re-exported from Elm.Basics:

#### `Tuple`

``` purescript
data Tuple a b
  = Tuple a b
```

A simple product type for wrapping a pair of component values.

##### Instances
``` purescript
(Show a, Show b) => Show (Tuple a b)
(Eq a, Eq b) => Eq (Tuple a b)
(Eq a) => Eq1 (Tuple a)
(Ord a, Ord b) => Ord (Tuple a b)
(Ord a) => Ord1 (Tuple a)
(Bounded a, Bounded b) => Bounded (Tuple a b)
Semigroupoid Tuple
(Semigroup a, Semigroup b) => Semigroup (Tuple a b)
(Monoid a, Monoid b) => Monoid (Tuple a b)
(Semiring a, Semiring b) => Semiring (Tuple a b)
(Ring a, Ring b) => Ring (Tuple a b)
(CommutativeRing a, CommutativeRing b) => CommutativeRing (Tuple a b)
(HeytingAlgebra a, HeytingAlgebra b) => HeytingAlgebra (Tuple a b)
(BooleanAlgebra a, BooleanAlgebra b) => BooleanAlgebra (Tuple a b)
Functor (Tuple a)
Invariant (Tuple a)
Bifunctor Tuple
(Semigroup a) => Apply (Tuple a)
Biapply Tuple
(Monoid a) => Applicative (Tuple a)
Biapplicative Tuple
(Semigroup a) => Bind (Tuple a)
(Monoid a) => Monad (Tuple a)
Extend (Tuple a)
Comonad (Tuple a)
(Lazy a, Lazy b) => Lazy (Tuple a b)
Foldable (Tuple a)
Bifoldable Tuple
Traversable (Tuple a)
Bitraversable Tuple
(TypeEquals a Unit) => Distributive (Tuple a)
```

#### `Order`

``` purescript
type Order = Ordering
```

Represents the relative ordering of two things.
The relations are less than, equal to, and greater than.

Equivalent to Purescript's `Ordering`.

#### `Never`

``` purescript
type Never = Void
```

A value that can never happen! For context:

  - The boolean type `Bool` has two values: `True` and `False`
  - The unit type `()` has one value: `()`
  - The never type `Never` has no values!

You may see it in the wild in `Html Never` which means this HTML will never
produce any messages. You would need to write an event handler like
`onClick ??? : Attribute Never` but how can we fill in the question marks?!
So there cannot be any event handlers on that HTML.

You may also see this used with tasks that never fail, like `Task Never ()`.

The `Never` type is useful for restricting *arguments* to a function. Maybe my
API can only accept HTML without event handlers, so I require `Html Never` and
users can give `Html msg` and everything will go fine. Generally speaking, you
do not want `Never` in your return types though.

This type was introduced in Elm 0.17.

The Purescript equivalent is `Void`.

#### `Float`

``` purescript
type Float = Number
```

The Purescript equivalent of Elm's `Float` is `Number`.

#### `Bool`

``` purescript
type Bool = Boolean
```

The Purescript equivalent of Elm's `Bool` is `Boolean`.

#### `Pow`

``` purescript
class Pow a  where
  pow :: a -> a -> a
```

A class for things that can be raised to a power.

##### Instances
``` purescript
Pow Int
Pow Number
```

#### `xor`

``` purescript
xor :: forall a. BooleanAlgebra a => a -> a -> a
```

The exclusive-or operator. `true` if exactly one input is `true`.

#### `uncurry`

``` purescript
uncurry :: forall a b c. (a -> b -> c) -> Tuple a b -> c
```

Turn a function of two arguments into a function that expects a tuple.

#### `turns`

``` purescript
turns :: Float -> Float
```

Convert turns to standard Elm angles (radians).
One turn is equal to 360&deg;.

#### `truncate`

``` purescript
truncate :: Float -> Int
```

Truncate a number, rounding towards zero.

#### `toString`

``` purescript
toString :: forall a. Show a => a -> String
```

Turn any kind of value into a string.

    toString 42 == "42"
    toString [1,2] == "[1,2]"
    toString "he said, \"hi\"" == "\"he said, \\\"hi\\\"\""

Equivalent to Purescript's `show`.

#### `toPolar`

``` purescript
toPolar :: Tuple Float Float -> Tuple Float Float
```

Convert Cartesian coordinates `Tuple x y` to polar coordinates `Tuple r theta`.

*Note that it would normally be better to use a record type here, rather than
tuples. However, it seems best to match the Elm API as closely as possible.*

*If you want some more sophisticated handling of complex numbers, see
[purescript-complex](http://pursuit.purescript.org/packages/purescript-complex).*

#### `toFloat`

``` purescript
toFloat :: Int -> Float
```

Convert an integer into a float.

Equivalent to Purescript's `toNumber`.

#### `tan`

``` purescript
tan :: Radians -> Number
```

Returns the tangent of the argument.

#### `sqrt`

``` purescript
sqrt :: Number -> Number
```

Returns the square root of the argument.

#### `snd`

``` purescript
snd :: forall a b. Tuple a b -> b
```

Returns the second component of a tuple.

#### `sin`

``` purescript
sin :: Radians -> Number
```

Returns the sine of the argument.

#### `round`

``` purescript
round :: Number -> Int
```

Convert a `Number` to an `Int`, by taking the nearest integer to the
argument. Values outside the `Int` range are clamped, `NaN` and `Infinity`
values return 0.

#### `rem`

``` purescript
rem :: forall a. EuclideanRing a => a -> a -> a
```

Find the remainder after dividing one number by another.

    rem 11 4 == 3
    rem 12 4 == 0
    rem 13 4 == 1
    rem -1 4 == -1

Equivalent to Purescript's `Prelude.mod`.

#### `radians`

``` purescript
radians :: Float -> Float
```

Convert radians to standard Elm angles (radians).

#### `pi`

``` purescript
pi :: Number
```

The ratio of the circumference of a circle to its diameter, around 3.14159.

#### `not`

``` purescript
not :: forall a. HeytingAlgebra a => a -> a
```

#### `never`

``` purescript
never :: forall a. Never -> a
```

A function that can never be called. Seems extremely pointless, but it
*can* come in handy. Imagine you have some HTML that should never produce any
messages. And say you want to use it in some other HTML that *does* produce
messages. You could say:

    import Html exposing (..)

    embedHtml :: Html Never -> Html msg
    embedHtml staticStuff =
      div []
        [ text "hello"
        , Html.map never staticStuff
        ]

So the `never` function is basically telling the type system, make sure no one
ever calls me!

The Purescript equivalent is `absurd`.

This function was added in Elm 0.18.

#### `negate`

``` purescript
negate :: forall a. Ring a => a -> a
```

`negate x` can be used as a shorthand for `zero - x`.

#### `mod`

``` purescript
mod :: forall a. Ord a => EuclideanRing a => a -> a -> a
```

Perform [modular arithmetic](http://en.wikipedia.org/wiki/Modular_arithmetic).

       7 % 2 == 1
    (-1) % 4 == 3

Note that this is not the same as Purescript's `Prelude.mod` --
for that, see `Basics.rem`.

#### `min`

``` purescript
min :: forall a. Ord a => a -> a -> a
```

Take the minimum of two values. If they are considered equal, the first
argument is chosen.

#### `max`

``` purescript
max :: forall a. Ord a => a -> a -> a
```

Take the maximum of two values. If they are considered equal, the first
argument is chosen.

#### `logBase`

``` purescript
logBase :: Float -> Float -> Float
```

Calculate the logarithm of a number with a given base.

    logBase 10.0 100.0 == 2.0
    logBase 2.0 256.0 == 8.0

#### `isNaN`

``` purescript
isNaN :: Number -> Boolean
```

Test whether a number is NaN

#### `isInfinite`

``` purescript
isInfinite :: Float -> Bool
```

Determine whether a float is positive or negative infinity.

    isInfinite (0.0 / 0.0)   == false
    isInfinite (sqrt (-1.0)) == false
    isInfinite (1.0 / 0.0)   == true
    isInfinite 1.0           == false

Notice that NaN is not infinite! For float `n` to be finite implies that
`not (isInfinite n || isNaN n)` evaluates to `true`.

Note that this is not equivalent to the negation of Javascript's `isFinite()`.

#### `intDiv`

``` purescript
intDiv :: forall a. EuclideanRing a => a -> a -> a
```

Integer division. The remainder is discarded.

In Purescript, you can simply use `/`.

#### `identity`

``` purescript
identity :: forall a. a -> a
```

Given a value, returns exactly the same value. This is called
[the identity function](http://en.wikipedia.org/wiki/Identity_function).

The Purescript equivalent is `id`.

#### `fst`

``` purescript
fst :: forall a b. Tuple a b -> a
```

Returns the first component of a tuple.

#### `fromPolar`

``` purescript
fromPolar :: Tuple Float Float -> Tuple Float Float
```

Convert polar coordinates `Tuple r theta` to Cartesian coordinates `Tuple x y`.

*Note that it would normally be better to use a record type here, rather than
tuples. However, it seems best to match the Elm API as closely as possible.*

*If you want some more sophisticated handling of complex numbers, see
[purescript-complex](http://pursuit.purescript.org/packages/purescript-complex).*

#### `floor`

``` purescript
floor :: Number -> Int
```

Convert a `Number` to an `Int`, by taking the closest integer equal to or
less than the argument. Values outside the `Int` range are clamped, `NaN`
and `Infinity` values return 0.

#### `flip`

``` purescript
flip :: forall a b c. (a -> b -> c) -> b -> a -> c
```

Flips the order of the arguments to a function of two arguments.

```purescript
flip const 1 2 = const 2 1 = 2
```

#### `e`

``` purescript
e :: Number
```

The base of natural logarithms, *e*, around 2.71828.

#### `degrees`

``` purescript
degrees :: Float -> Float
```

Convert degrees to standard Elm angles (radians).

#### `curry`

``` purescript
curry :: forall a b c. (Tuple a b -> c) -> a -> b -> c
```

Turn a function that expects a tuple into a function of two arguments.

#### `cos`

``` purescript
cos :: Radians -> Number
```

Returns the cosine of the argument.

#### `composeFlipped`

``` purescript
composeFlipped :: forall a b c. (a -> b) -> (b -> c) -> (a -> c)
```

Function composition, passing results along in the suggested direction. For
example, the following code checks if the square root of a number is odd:

    sqrt >> isEven >> not

This direction of function composition seems less pleasant than `(<<)` which
reads nicely in expressions like: `filter (not << isRegistered) students`

Equivalent to Purescript's `>>>`.

#### `compose`

``` purescript
compose :: forall a b c. (b -> c) -> (a -> b) -> (a -> c)
```

Function composition, passing results along in the suggested direction. For
example, the following code checks if the square root of a number is odd:

    not << isEven << sqrt

You can think of this operator as equivalent to the following:

    (g << f)  ==  (\x -> g (f x))

So our example expands out to something like this:

    \n -> not (isEven (sqrt n))

Equivalent to Purescript's `<<<`.

#### `compare`

``` purescript
compare :: forall a. Ord a => a -> a -> Ordering
```

#### `clamp`

``` purescript
clamp :: forall a. Ord a => a -> a -> a -> a
```

Clamp a value between a minimum and a maximum. For example:

``` purescript
let f = clamp 0 10
f (-5) == 0
f 5    == 5
f 15   == 10
```

#### `ceiling`

``` purescript
ceiling :: Float -> Int
```

Ceiling function, rounding up.

Equivalent to Purescript's `ceil`.

#### `atan2`

``` purescript
atan2 :: Number -> Number -> Radians
```

Four-quadrant tangent inverse. Given the arguments `y` and `x`, returns
the inverse tangent of `y / x`, where the signs of both arguments are used
to determine the sign of the result.
If the first argument is negative, the result will be negative.
The result is the angle between the positive x axis and  a point `(x, y)`.

#### `atan`

``` purescript
atan :: Number -> Radians
```

Returns the inverse tangent of the argument.

#### `asin`

``` purescript
asin :: Number -> Radians
```

Returns the inverse sine of the argument.

#### `applyFnFlipped`

``` purescript
applyFnFlipped :: forall a b. a -> (a -> b) -> b
```

Forward function application `x |> f == f x`. This function is useful
for avoiding parentheses and writing code in a more natural way.
Consider the following code to create a pentagon:

    scale 2 (move (10,10) (filled blue (ngon 5 30)))

This can also be written as:

    ngon 5 30
      |> filled blue
      |> move (10,10)
      |> scale 2

Equivalent to Purescript's `#`.

#### `applyFn`

``` purescript
applyFn :: forall a b. (a -> b) -> a -> b
```

Backward function application `f <| x == f x`. This function is useful for
avoiding parentheses. Consider the following code to create a text element:

    leftAligned (monospace (fromString "code"))

This can also be written as:

    leftAligned <| monospace <| fromString "code"

Equivalent to Purescript's `$`.

#### `always`

``` purescript
always :: forall a b. a -> b -> a
```

Create a function that *always* returns the same value. Useful with
functions like `map`:

    List.map (always 0) [1,2,3,4,5] == [0,0,0,0,0]

    -- List.map (\_ -> 0) [1,2,3,4,5] == [0,0,0,0,0]
    -- always = (\x _ -> x)

The Purescript equivalent is `const`.

#### `acos`

``` purescript
acos :: Number -> Radians
```

Returns the inverse cosine of the argument.

#### `abs`

``` purescript
abs :: forall a. Ring a => Ord a => a -> a
```

Take the absolute value of a number.

#### `(||)`

``` purescript
infixr 2 disj as ||
```

#### `(|>)`

``` purescript
infixl 0 applyFnFlipped as |>
```

#### `(^)`

``` purescript
infixr 8 pow as ^
```

#### `(>>)`

``` purescript
infixl 9 composeFlipped as >>
```

#### `(>=)`

``` purescript
infixl 4 greaterThanOrEq as >=
```

#### `(>)`

``` purescript
infixl 4 greaterThan as >
```

#### `(==)`

``` purescript
infix 4 eq as ==
```

#### `(<|)`

``` purescript
infixr 0 applyFn as <|
```

#### `(<=)`

``` purescript
infixl 4 lessThanOrEq as <=
```

#### `(<<)`

``` purescript
infixr 9 compose as <<
```

#### `(<)`

``` purescript
infixl 4 lessThan as <
```

#### `(/=)`

``` purescript
infix 4 notEq as /=
```

#### `(//)`

``` purescript
infixl 7 intDiv as //
```

#### `(/)`

``` purescript
infixl 7 div as /
```

#### `(-)`

``` purescript
infixl 6 sub as -
```

#### `(++)`

``` purescript
infixl 5 append as ++
```

#### `(+)`

``` purescript
infixl 6 add as +
```

#### `(*)`

``` purescript
infixl 7 mul as *
```

#### `(&&)`

``` purescript
infixr 3 conj as &&
```

#### `(%)`

``` purescript
infixl 7 mod as %
```

### Re-exported from Elm.Monoid:

#### `none`

``` purescript
none :: forall m. Monoid m => m
```

Produce an "empty" value of the relevant type.

Equivalent to Purescript's `mempty`.

### Re-exported from Elm.Platform:

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

### Re-exported from Elm.Platform.Cmd:

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

#### `(!)`

``` purescript
infixl 5 withCmds as !
```

### Re-exported from Elm.Platform.Sub:

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

### Re-exported from Elm.Result:

#### `Result`

``` purescript
data Result error value
  = Ok value
  | Err error
```

A `Result` is either `Ok` meaning the computation succeeded, or it is an
`Err` meaning that there was some failure.

##### Instances
``` purescript
Functor (Result a)
Bifunctor Result
Apply (Result e)
Applicative (Result e)
Alt (Result e)
Bind (Result e)
Monad (Result e)
Extend (Result e)
(Show a, Show b) => Show (Result a b)
(Eq a, Eq b) => Eq (Result a b)
(Ord a, Ord b) => Ord (Result a b)
(Bounded a, Bounded b) => Bounded (Result a b)
Foldable (Result a)
Bifoldable Result
Traversable (Result a)
Bitraversable Result
(Semiring b) => Semiring (Result a b)
(Semigroup b) => Semigroup (Result a b)
```

