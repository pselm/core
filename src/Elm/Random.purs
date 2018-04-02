
-- | This library helps you generate pseudo-random values.
-- |
-- | This library is all about building generators for whatever
-- | type of values you need. There are a bunch of primitive generators like
-- | [`bool`](#v:bool) and [`int`](#v:int) that you can build up into fancier
-- | generators with functions like [`list`](#v:list) and [`map`](#v:map).
-- |
-- | It may be helpful to
-- | [read about JSON decoders](https://evancz.gitbooks.io/an-introduction-to-elm/content/interop/json.html)
-- | because they work very similarly.
-- |
-- | > *Note:* This is an implementation of the Portable Combined Generator of
-- | L'Ecuyer for 32-bit computers. It is almost a direct translation from the
-- | [System.Random](http://hackage.haskell.org/package/random-1.0.1.1/docs/System-Random.html)
-- | module. It has a period of roughly 2.30584e18.
-- |
-- | *This is a translation of the Elm code to Purescript. I suppose the more idiomatic
-- | Purescript way of doing things like this would be to use the `Arbitrary` class
-- | (and other facilities) in the
-- | [purescript-quickcheck](https://pursuit.purescript.org/packages/purescript-quickcheck)
-- | library.*

-- Note that the code here is substantially derived from Elm's `Random` module. Please
-- see the LICENSE file for the Elm license.

module Elm.Random
    ( module Virtual
    , Generator, Seed
    , bool, int, float
    , list, pair
    , minInt, maxInt
    , generate, initialSeed
    ) where


-- For re-export

import Elm.Apply (map2, map3, map4, map5) as Virtual
import Elm.Bind (andThen) as Virtual
import Prelude (map) as Virtual


-- Internal

import Prelude
    ( (==), (/), (*), (-), (+), (<), ($), (<>)
    , negate, zero, one
    , class Ord, class Functor, class Apply, class Bind, class Semigroup
    , class Applicative, pure
    )

import Elm.Apply (map2)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Ord (max)
import Data.Monoid (class Monoid, mempty)
import Elm.Basics ((%), Bool, Float)
import Data.Int53 (class Int53Value, Int53, fromInt, fromInt53, toInt53, round, toNumber)


-- | Create a generator that produces boolean values. The following example
-- | simulates a coin flip that may land heads or tails.
-- |
-- |     data Flip = Heads | Tails
-- |
-- |     coinFlip :: Generator Flip
-- |     coinFlip =
-- |         map (\b -> if b then Heads else Tails) bool
bool :: Generator Bool
bool =
    map ((==) 1) (int 0 1)


-- | Generate 32-bit integers in a given range.
-- |
-- |     int 0 10   -- an integer between zero and ten
-- |     int (-5) 5   -- an integer between -5 and 5
-- |
-- |     int minInt maxInt  -- an integer in the widest range feasible
-- |
-- | You can supply either `Int` or `Int53` for the parameters.
int :: ∀ a. Ord a => Int53Value a => a -> a -> Generator a
int a b =
    Generator $ \seed ->
        let
            ordered =
                if a < b
                   then {lo: toInt53 a, hi: toInt53 b}
                   else {lo: toInt53 b, hi: toInt53 a}

            -- k needs to be a float, because it represents a positive number
            -- expressing the whole range between lo and hi. But that could
            -- overflow maxInt, and Purescript enforces maxInt, given what
            -- the compiler does for ints
            k =
                ordered.hi - ordered.lo + one

            -- 2^31 - 87
            base =
                fromInt 2147483561

            n =
                iLogBase base k

            f ns acc state =
                case ns of
                    0 ->
                        Tuple acc state

                    _ ->
                        let
                            x = fst tuple
                            state' = snd tuple
                            tuple = next state

                        in
                            f (ns - one) (x + acc * base) state'

        in
            case f n one seed of
                Tuple v state' ->
                    { value: fromInt53 (ordered.lo + (v % k))
                    , seed: state'
                    }


iLogBase :: Int53 -> Int53 -> Int
iLogBase b i =
    go b i 1
        where
            go base value accum =
                if value < base
                   then accum
                   else go base (value / base) (accum + 1)


-- | The maximum value for randomly generated 32-bit ints: 2147483647.
maxInt :: Int53
maxInt = fromInt 2147483647


-- | The minimum value for randomly generated 32-bit ints: -2147483648.
minInt :: Int53
minInt = round (-2147483648.0)


-- | Generate floats in a given range. The following example is a generator
-- | that produces decimals between 0 and 1.
-- |
-- |     probability :: Generator Float
-- |     probability =
-- |         float 0 1
float :: Float -> Float -> Generator Float
float a b =
    Generator $ \seed ->
        let
            ordered =
                if a < b
                   then {lo: a, hi: b}
                   else {lo: b, hi: a}

            generated =
                generate (int minInt maxInt) seed

            negativeOneToOne =
                (toNumber generated.value) / toNumber (maxInt - minInt)

            scaled =
                (ordered.lo + ordered.hi) / 2.0 + ((ordered.hi - ordered.lo) * negativeOneToOne)

         in
            { value: scaled
            , seed: generated.seed
            }


-- DATA STRUCTURES

-- | Create a pair of random values. A common use of this might be to generate
-- | a point in a certain 2D space. Imagine we have a collage that is 400 pixels
-- | wide and 200 pixels tall.
-- |
-- |     randomPoint :: Generator (Tuple Int Int)
-- |     randomPoint =
-- |         pair (int -200 200) (int -100 100)
pair :: ∀ a b. Generator a -> Generator b -> Generator (Tuple a b)
pair genA genB =
    map2 Tuple genA genB


-- | Create a list of random values.
-- |
-- |     floatList :: Generator (List Float)
-- |     floatList =
-- |         list 10 (float 0 1)
-- |
-- |     intList :: Generator (List Int)
-- |     intList =
-- |         list 5 (int 0 100)
-- |
-- |     intPairs :: Generator (List (Tuple Int Int))
-- |     intPairs =
-- |         list 10 <| pair (int 0 100) (int 0 100)
-- |
-- | The return type is polymorphic in order to accommodate `List` or `Array`, among others.
list :: ∀ t a. Monoid (t a) => Applicative t => Int -> Generator a -> Generator (t a)
list n (Generator gen) =
    Generator $ \seed ->
        go mempty n seed

        where
            go memo n seed =
                if n < 1
                    then
                        { value: memo
                        , seed: seed
                        }
                    else
                        let
                            result =
                                gen seed

                        in
                            go (memo <> (pure result.value)) (n - 1) result.seed


-- | Transform the values produced by a generator. The following examples show
-- | how to generate booleans and letters based on a basic integer generator.
-- |
-- |     bool :: Generator Bool
-- |     bool =
-- |       map ((==) 1) (int 0 1)
-- |
-- |     lowercaseLetter :: Generator Char
-- |     lowercaseLetter =
-- |       map (\n -> Char.fromCode (n + 97)) (int 0 25)
-- |
-- |     uppercaseLetter :: Generator Char
-- |     uppercaseLetter =
-- |       map (\n -> Char.fromCode (n + 65)) (int 0 25)
map :: ∀ a b. (a -> b) -> Generator a -> Generator b
map func (Generator genA) =
    Generator $ \seed0 ->
        let
            result = genA seed0

        in
            { value: func result.value
            , seed: result.seed
            }

instance functorGenerator :: Functor Generator where
    map = map

instance applyGenerator :: Apply Generator where
    apply = apply

apply :: ∀ a b. Generator (a -> b) -> Generator a -> Generator b
apply (Generator genAB) (Generator genA) =
    Generator $ \seed ->
        let
            func =
                genAB seed

            a =
                genA func.seed

        in
            { value: func.value a.value
            , seed: a.seed
            }


-- | Chain random operations, threading through the seed. In the following
-- | example, we will generate a random letter by putting together uppercase and
-- | lowercase letters.
-- |
-- |     letter :: Generator Char
-- |     letter =
-- |       bool `andThen` \b ->
-- |         if b then uppercaseLetter else lowercaseLetter
-- |
-- |     -- bool :: Generator Bool
-- |     -- uppercaseLetter :: Generator Char
-- |     -- lowercaseLetter :: Generator Char
andThen :: ∀ a b. Generator a -> (a -> Generator b) -> Generator b
andThen (Generator gen) callback =
    Generator $ \seed ->
        let
            result =
                gen seed

        in
            case callback result.value of
                Generator genB ->
                    genB result.seed

instance bindGenerator :: Bind Generator where
    bind = andThen


instance semigroupGenerator :: (Semigroup a) => Semigroup (Generator a) where
    append = append

append :: ∀ a. (Semigroup a) => Generator a -> Generator a -> Generator a
append (Generator genA) (Generator genB) =
    Generator $ \seed ->
        let
            result1 = genA seed
            result2 = genB result1.seed

        in
            { value: result1.value <> result2.value
            , seed: result2.seed
            }


-- | A `Generator` is like a recipe for generating certain random values. So a
-- | `Generator Int` describes how to generate integers and a `Generator String`
-- | describes how to generate strings.
-- |
-- | To actually *run* a generator and produce the random values, you need to use
-- | functions like [`generate`](#v:generate) and [`initialSeed`](#v:initialSeed).
data Generator a = Generator (Seed -> Generated a)


type Generated a =
    { value :: a
    , seed :: Seed
    }


-- | A `Seed` is the source of randomness in this whole system. Whenever
-- | you want to use a generator, you need to pair it with a seed.
data Seed = Seed Int53 Int53


-- | Generate a random value as specified by a given `Generator`.
-- |
-- | In the following example, we are trying to generate a number between 0 and 100
-- | with the `int 0 100` generator. Each time we call `generate` we need to provide
-- | a seed. This will produce a random number and a *new* seed to use if we want to
-- | run other generators later.
-- |
-- | So here it is done right, where we get a new seed from each `generate` call and
-- | thread that through.
-- |
-- |     seed0 = initialSeed 31415
-- |
-- |     -- generate (int 0 100) seed0 ==> {value: 42, seed: seed1}
-- |     -- generate (int 0 100) seed1 ==> {value: 31, seed: seed2}
-- |     -- generate (int 0 100) seed2 ==> (value: 99, seed: seed3}
-- |
-- | Notice that we use different seeds on each line. This is important! If you use
-- | the same seed, you get the same results.
-- |
-- |     -- generate (int 0 100) seed0 ==> {value: 42, seed: seed1}
-- |     -- generate (int 0 100) seed0 ==> {value: 42, seed: seed1}
-- |     -- generate (int 0 100) seed0 ==> {value: 42, seed: seed1}
generate :: ∀ a. Generator a -> Seed -> Generated a
generate (Generator generator) seed =
    generator seed


-- | Create a &ldquo;seed&rdquo; of randomness which makes it possible to
-- | generate random values. If you use the same seed many times, it will result
-- | in the same thing every time!
-- |
-- | You can supply either an `Int` or `Int53` for the parameter.
initialSeed :: ∀ a. (Int53Value a) => a -> Seed
initialSeed n =
    initState (toInt53 n)


-- | Produce the initial generator state. Distinct arguments should be likely
-- | to produce distinct generator states.
initState :: Int53 -> Seed
initState s' =
    let
        s =
            max s' (-s')

        q =
            s / (magicNum6 - one)

        s1 =
            s % (magicNum6 - one)

        s2 =
            q % (magicNum7 - one)

    in
        Seed (s1 + one) (s2 + one)


magicNum0 :: Int53
magicNum0 = fromInt 40014

magicNum1 :: Int53
magicNum1 = fromInt 53668

magicNum2 :: Int53
magicNum2 = fromInt 12211

magicNum3 :: Int53
magicNum3 = fromInt 52774

magicNum4 :: Int53
magicNum4 = fromInt 40692

magicNum5 :: Int53
magicNum5 = fromInt 3791

magicNum6 :: Int53
magicNum6 = fromInt 2147483563

magicNum7 :: Int53
magicNum7 = fromInt 2137383399

magicNum8 :: Int53
magicNum8 = fromInt 2147483562


next :: Seed -> Tuple Int53 Seed
next (Seed s1 s2) =
    -- Div always rounds down and so random numbers are biased
    -- ideally we would use division that rounds towards zero so
    -- that in the negative case it rounds up and in the positive case
    -- it rounds down. Thus half the time it rounds up and half the time it
    -- rounds down
    let
        k = s1 / magicNum1
        s1' = magicNum0 * (s1 - k * magicNum1) - k * magicNum2
        s1'' = if s1' < zero then s1' + magicNum6 else s1'
        k' = s2 / magicNum3
        s2' = magicNum4 * (s2 - k' * magicNum3) - k' * magicNum5
        s2'' = if s2' < zero then s2' + magicNum7 else s2'
        z = s1'' - s2''
        z' = if z < one then z + magicNum8 else z

    in
        Tuple z' (Seed s1'' s2'')
