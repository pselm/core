
-- | The Elm equivalent of Purescript's `Prelude`.
-- |
-- | Note that many of these functions are re-exported from other modules in
-- | Purescript. The goal is to bundle together the same things that Elm's `Basics` module
-- | bundles together.
-- |
-- | In Elm, these functions are all imported by default. Purescript does not
-- | have default imports, so you will need to import this manually.
-- |
-- | ### Tuples
-- |
-- | Note that Purescript does not have a literal syntax for Tuples. So, in places where
-- | you used Tuples, there are two alternatives.
-- |
-- | * There is a `Data.Tuple` type in
-- |   [purescript-tuples](http://pursuit.purescript.org/packages/purescript-tuples).
-- |   However, it is just an ordinary tagged union type. So you construct and pattern
-- |   match it in the usual way -- there is no `,` operator to construct tuples.
-- |
-- |       tuple = Tuple 1 2
-- |
-- |       case tuple of
-- |           Tuple a b -> a
-- |
-- | * Usually, it's better to use Purescript's record type. Essentially, if you
-- |   have a `Tuple2`, just use a record where you name the first and second
-- |   elements.  (You could even name them `fst` and `snd` if you don't have
-- |   anything better at hand).
-- |
-- |       tuple =
-- |           { fst: 1
-- |           , snd: 2
-- |           }
-- |
-- | In converting Elm APIs, I have consistently used `Data.Tuple`. (I had originally
-- | sometimes used records instead. However, on reflection, it seemed best to keep the
-- | translation from Elm to Purescript as mechanical as possible.)


module Elm.Basics
    ( module Virtual
    , Order(..)
    , class Pow, pow, (^)
    , abs, xor
    , intDiv, (//)
    , rem, mod, (%)
    , Float, logBase, truncate, ceiling, toFloat
    , radians, degrees, turns, fromPolar, toPolar
    , toString, isInfinite
    , identity, always
    , applyFn, (<|)
    , applyFnFlipped, (|>)
    , compose, (<<)
    , composeFlipped, (>>)
    , (++)
    , Bool
    ) where


-- For re-export
import Prelude
    ( (*), (+), (-), (/), negate
    , compare
    , (==), (/=), (<), (>), (<=), (>=)
    , (&&), (||), not
    , flip
    ) as Virtual

import Data.Ord (max, min, clamp) as Virtual
import Global (isNaN) as Virtual
import Data.Tuple (Tuple(..), fst, snd, curry, uncurry) as Virtual
import Math (sqrt, e, pi, sin, cos, tan, acos, asin, atan, atan2) as Virtual
import Data.Int (round, floor) as Virtual


-- For internal use
import Prelude
    ( id, (<<<), (>>>), ($), (#), const
    , (*), (/), (+)
    , (>), (>=), (==)
    , (||), (&&)
    , append
    , class Ring, negate, not, zero
    , class Show, show
    , class EuclideanRing
    , class Ord, Ordering
    , class BooleanAlgebra
    )

import Prelude as Prelude
import Global as Global
import Math (cos, sqrt, log, pi, sin, atan2)
import Math as Math
import Data.Int (round, toNumber)
import Data.Int as Int
import Data.Tuple (Tuple(..))
import Data.Void (Void)
import Elm.Debug (crash)


-- | The Purescript equivalent of Elm's `Float` is `Number`.
type Float = Number


-- | The Purescript equivalent of Elm's `Bool` is `Boolean`.
type Bool = Boolean


-- | Convert radians to standard Elm angles (radians).
radians :: Float -> Float
radians = id


-- | Convert degrees to standard Elm angles (radians).
degrees :: Float -> Float
degrees d =
    d * pi / 180.0


-- | Convert turns to standard Elm angles (radians).
-- | One turn is equal to 360&deg;.
turns :: Float -> Float
turns t =
    2.0 * pi * t


-- | Convert polar coordinates `Tuple r theta` to Cartesian coordinates `Tuple x y`.
-- |
-- | *Note that it would normally be better to use a record type here, rather than
-- | tuples. However, it seems best to match the Elm API as closely as possible.*
-- |
-- | *If you want some more sophisticated handling of complex numbers, see
-- | [purescript-complex](http://pursuit.purescript.org/packages/purescript-complex).*
fromPolar :: Tuple Float Float -> Tuple Float Float
fromPolar (Tuple r theta) =
    Tuple
        (r * cos theta)
        (r * sin theta)


-- | Convert Cartesian coordinates `Tuple x y` to polar coordinates `Tuple r theta`.
-- |
-- | *Note that it would normally be better to use a record type here, rather than
-- | tuples. However, it seems best to match the Elm API as closely as possible.*
-- |
-- | *If you want some more sophisticated handling of complex numbers, see
-- | [purescript-complex](http://pursuit.purescript.org/packages/purescript-complex).*
toPolar :: Tuple Float Float -> Tuple Float Float
toPolar (Tuple x y) =
    Tuple
        (sqrt (x * x + y * y))
        (atan2 y x)


infixl 5 append as ++

infixl 7 intDiv as //

-- | Integer division. The remainder is discarded.
-- |
-- | In Purescript, you can simply use `/`.
intDiv :: ∀ a. (EuclideanRing a) => a -> a -> a
intDiv = Prelude.div


-- I'd like to do the following, but it looks like it doesn't work.
--
-- infixl 7 `rem`

-- | Find the remainder after dividing one number by another.
-- |
-- |     7 `rem` 2 == 1
-- |     -1 `rem` 4 == -1
-- |
-- | Equivalent to Purescript's `Prelude.mod`.
rem :: ∀ a. (EuclideanRing a) => a -> a -> a
rem = Prelude.mod


infixl 7 mod as %

-- | Perform [modular arithmetic](http://en.wikipedia.org/wiki/Modular_arithmetic).
-- |
-- |        7 % 2 == 1
-- |     (-1) % 4 == 3
-- |
-- | Note that this is not the same as Purescript's `Prelude.mod` --
-- | for that, see `Basics.rem`.
mod :: ∀ a. Ord a => EuclideanRing a => a -> a -> a
mod a b =
    let
        r :: a
        r = a `rem` b

        m :: a
        m =
            if a == zero
                then zero
                else
                    if b == zero
                        then crash "Cannot perform mod 0. Division by zero error."
                        else
                            if b > zero
                                then
                                    if a >= zero
                                        then r
                                        else r + b
                                else
                                    -((-a) % (-b))

    in
        if m == b
            then zero
            else m


-- | A class for things that can be raised to a power.
class Pow a where
    pow :: a -> a -> a

instance intIntPow :: Pow Int where
    pow a b = round <| Math.pow (toNumber a) (toNumber b)

instance floatFloatPow :: Pow Number where
    pow a b = Math.pow a b


infixr 8 pow as ^


-- | Take the absolute value of a number.
abs :: ∀ a. Ring a => Ord a => a -> a
abs a =
    if a >= zero
        then a
        else negate a


-- | Calculate the logarithm of a number with a given base.
-- |
-- |     logBase 10.0 100.0 == 2.0
-- |     logBase 2.0 256.0 == 8.0
logBase :: Float -> Float -> Float
logBase base n =
    log n / log base


-- | Represents the relative ordering of two things.
-- | The relations are less than, equal to, and greater than.
-- |
-- | Equivalent to Purescript's `Ordering`.
type Order = Ordering


-- | The exclusive-or operator. `true` if exactly one input is `true`.
xor :: ∀ a. (BooleanAlgebra a) => a -> a -> a
xor a b =
    (a && not b) || (not a && b)


-- | Truncate a number, rounding towards zero.
foreign import truncate :: Float -> Int


-- | Ceiling function, rounding up.
-- |
-- | Equivalent to Purescript's `ceil`.
ceiling :: Float -> Int
ceiling = Int.ceil


-- | Convert an integer into a float.
-- |
-- | Equivalent to Purescript's `toNumber`.
toFloat :: Int -> Float
toFloat = Int.toNumber


-- | Determine whether a float is positive or negative infinity.
-- |
-- |     isInfinite (0.0 / 0.0)   == false
-- |     isInfinite (sqrt (-1.0)) == false
-- |     isInfinite (1.0 / 0.0)   == true
-- |     isInfinite 1.0           == false
-- |
-- | Notice that NaN is not infinite! For float `n` to be finite implies that
-- | `not (isInfinite n || isNaN n)` evaluates to `true`.
-- |
-- | Note that this is not equivalent to the negation of Javascript's `isFinite()`.
isInfinite :: Float -> Bool
isInfinite n =
    n == Global.infinity || n == (-Global.infinity)


-- | Turn any kind of value into a string.
-- |
-- |     toString 42 == "42"
-- |     toString [1,2] == "[1,2]"
-- |     toString "he said, \"hi\"" == "\"he said, \\\"hi\\\"\""
-- |
-- | Equivalent to Purescript's `show`.
toString :: ∀ a. (Show a) => a -> String
toString = show


-- Function Helpers


infixr 9 compose as <<

-- | Function composition, passing results along in the suggested direction. For
-- | example, the following code checks if the square root of a number is odd:
-- |
-- |     not << isEven << sqrt
-- |
-- | You can think of this operator as equivalent to the following:
-- |
-- |     (g << f)  ==  (\x -> g (f x))
-- |
-- | So our example expands out to something like this:
-- |
-- |     \n -> not (isEven (sqrt n))
-- |
-- | Equivalent to Purescript's `<<<`.
compose :: ∀ a b c. (b -> c) -> (a -> b) -> (a -> c)
compose = (<<<)


infixl 9 composeFlipped as >>

-- | Function composition, passing results along in the suggested direction. For
-- | example, the following code checks if the square root of a number is odd:
-- |
-- |     sqrt >> isEven >> not
-- |
-- | This direction of function composition seems less pleasant than `(<<)` which
-- | reads nicely in expressions like: `filter (not << isRegistered) students`
-- |
-- | Equivalent to Purescript's `>>>`.
composeFlipped :: ∀ a b c. (a -> b) -> (b -> c) -> (a -> c)
composeFlipped = (>>>)


infixl 0 applyFnFlipped as |>

-- | Forward function application `x |> f == f x`. This function is useful
-- | for avoiding parentheses and writing code in a more natural way.
-- | Consider the following code to create a pentagon:
-- |
-- |     scale 2 (move (10,10) (filled blue (ngon 5 30)))
-- |
-- | This can also be written as:
-- |
-- |     ngon 5 30
-- |       |> filled blue
-- |       |> move (10,10)
-- |       |> scale 2
-- |
-- | Equivalent to Purescript's `#`.
applyFnFlipped :: ∀ a b. a -> (a -> b) -> b
applyFnFlipped = (#)


infixr 0 applyFn as <|

-- | Backward function application `f <| x == f x`. This function is useful for
-- | avoiding parentheses. Consider the following code to create a text element:
-- |
-- |     leftAligned (monospace (fromString "code"))
-- |
-- | This can also be written as:
-- |
-- |     leftAligned <| monospace <| fromString "code"
-- |
-- | Equivalent to Purescript's `$`.
applyFn :: ∀ a b. (a -> b) -> a -> b
applyFn = ($)


-- | Given a value, returns exactly the same value. This is called
-- | [the identity function](http://en.wikipedia.org/wiki/Identity_function).
-- |
-- | The Purescript equivalent is `id`.
identity :: ∀ a. a -> a
identity = id


-- | Create a [constant function](http://en.wikipedia.org/wiki/Constant_function),
-- | a function that *always* returns the same value regardless of what input you give.
-- | It is defined as:
-- |
-- |     always a b = a
-- |
-- | It totally ignores the second argument, so `always 42` is a function that always
-- | returns 42. When you are dealing with higher-order functions, this comes in
-- | handy more often than you might expect. For example, creating a zeroed out list
-- | of length ten would be:
-- |
-- |     map (always 0) [0..9]
-- |
-- | The Purescript equivalent is `const`.
always :: ∀ a b. a -> b -> a
always = const


-- | A type that is "uninhabited". There are no values of type `Never`, and its
-- | primary use is demanding that certain tasks cannot possibly fail.
-- |
-- | For example, a task with type `(Task Never Int)` must *always* succeed with an
-- | integer. For the task to fail, someone would need to say `(Task.fail ???)` but
-- | since there is no value with type `Never` they could not fill in the question
-- | marks!
-- |
-- | This type was introduced in Elm 0.17.
-- |
-- | The Purescript equivalent is `Void`.
type Never = Void
