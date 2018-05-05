
-- | > A library for fast immutable arrays. The elements in an array must have the
-- | > same type.
-- |
-- | This is based on the Purescript `Data.Sequence` package -- an `Array` is a
-- | `Data.Sequence.Seq`, so you can use additional functions from that package
-- | as well.
-- |
-- | Note that the Purescript primitive `Array` type is something different --
-- | it is actually a Javascript array. So, if you're importing this unqualified,
-- | you'll probably need to do something like:
-- |
-- |     import Prim hiding (Array)
-- |
-- | to avoid an ambiguity.

module Elm.Array
    ( module Virtual
    , Array, fromList, toList, toIndexedList
    , push, get, set
    , slice, indexedMap, initialize
    ) where


-- For re-export

import Data.Sequence (filter, empty, length) as Virtual
import Data.Foldable (foldr) as Virtual
import Prelude (append, map) as Virtual
import Elm.Foldable (foldl) as Virtual
import Elm.Unfoldable (repeat) as Virtual

-- Internal

import Data.Sequence
    ( Seq, toUnfoldable, fromFoldable, empty, replace
    , snoc, index, null, take, drop, length
    )

-- Prevents conflict with the locally-defined `Array` type.
import Prim hiding (Array)

import Elm.Basics (Bool)
import Data.Array as DA
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.Unfoldable (class Unfoldable)
import Data.Ord (clamp)
import Elm.Maybe (Maybe)
import Data.Tuple (Tuple(..), snd)
import Data.Monoid (class Monoid, mempty)

import Prelude
    ( class Functor, (<$>)
    , class Applicative, pure
    , ($), (-), flip, (>=), (<=), (+), (<<<), (<>)
    )

-- | The `Array` type is synonym for `Data.Sequence.Seq`.
type Array = Seq


-- | > Initialize an array. `initialize n f` creates an array of length `n` with
-- | > the element at index `i` initialized to the result of `(f i)`.
-- | >
-- | >     initialize 4 identity    == fromList [0,1,2,3]
-- | >     initialize 4 (\n -> n*n) == fromList [0,1,4,9]
-- | >     initialize 4 (always 0)  == fromList [0,0,0,0]
initialize :: ∀ a. Int -> (Int -> a) -> Array a
initialize len func =
    if len <= 0
       then empty
       else
            func <$>
               fromFoldable (DA.range 0 (len - 1))


-- | > Create an array from a list.
-- |
-- | Note that this actually works with any `Foldable`.
fromList :: ∀ f a. (Foldable f) => f a -> Array a
fromList = fromFoldable


-- | > Create a list of elements from an array.
-- | >
-- | >     toList (fromList [3,5,8]) == [3,5,8]
-- |
-- | Note that this actually works with any type that is both a
-- | `Functor` and an `Unfoldable`.
toList :: ∀ f a. Functor f => Unfoldable f => Array a -> f a
toList = toUnfoldable


-- | > Create an indexed list from an array. Each element of the array will be
-- | > paired with its index.
-- | >
-- | >     toIndexedList (fromList ["cat","dog"]) == [Tuple 0 "cat", Tuple 1 "dog"]
-- |
-- | The container in the return type is defined polymorphically to accommodate
-- | `List` and Purescript's `Array`, among others.
toIndexedList :: ∀ f a. Applicative f => Monoid (f (Tuple Int a)) => Array a -> f (Tuple Int a)
toIndexedList arr =
    snd $ Foldable.foldr step (Tuple ((length arr) - 1) mempty) arr
        where
            step item (Tuple index list) =
                Tuple (index - 1) ((pure $ Tuple index item) <> list)


-- | > Apply a function on every element with its index as first argument.
-- | >
-- | >     indexedMap (*) (fromList [5,5,5]) == fromList [0,5,10]
indexedMap :: ∀ a b. (Int -> a -> b) -> Array a -> Array b
indexedMap func =
    snd <<< Foldable.foldl step (Tuple 0 empty)
        where
            step (Tuple index seq) item =
                Tuple (index + 1) (snoc seq (func index item))


-- | > Push an element to the end of an array.
-- | >
-- | >     push 3 (fromList [1,2]) == fromList [1,2,3]
-- |
-- | Equivalent to Purescript's `snoc`, but with the arguments flipped.
push :: ∀ a. a -> Array a -> Array a
push = flip snoc


-- | > Return Just the element at the index or Nothing if the index is out of range.
-- | >
-- | >     get  0 (fromList [0,5,3]) == Just 0
-- | >     get  2 (fromList [0,5,3]) == Just 3
-- | >     get  5 (fromList [0,5,3]) == Nothing
-- | >     get -1 (fromList [0,5,3]) == Nothing
-- |
-- | Equivalent to Purescript's `index`.
get :: ∀ a. Int -> Array a -> Maybe a
get = index


-- | > Set the element at a particular index. Returns an updated array.
-- | > If the index is out of range, the array is unaltered.
-- | >
-- | >     set 1 7 (fromList [1,2,3]) == fromList [1,7,3]
-- |
-- | Equivalent to Purescript's `replace`, but with the arguments flipped.
set :: ∀ a. Int -> a -> Array a -> Array a
set = flip replace


-- | > Get a sub-section of an array: `(slice start end array)`. The `start` is a
-- | > zero-based index where we will start our slice. The `end` is a zero-based index
-- | > that indicates the end of the slice. The slice extracts up to but not including
-- | > `end`.
-- | >
-- | >     slice  0  3 (fromList [0,1,2,3,4]) == fromList [0,1,2]
-- | >     slice  1  4 (fromList [0,1,2,3,4]) == fromList [1,2,3]
-- | >
-- | > Both the `start` and `end` indexes can be negative, indicating an offset from
-- | > the end of the array.
-- | >
-- | >     slice  1 -1 (fromList [0,1,2,3,4]) == fromList [1,2,3]
-- | >     slice -2  5 (fromList [0,1,2,3,4]) == fromList [3,4]
-- | >
-- | > This makes it pretty easy to `pop` the last element off of an array: `slice 0 -1 array`
slice :: ∀ a. Int -> Int -> Array a -> Array a
slice start end array =
    let
        len =
            length array

        clamper =
            clamp 0 len

        toDrop =
            clamper $
                if start >= 0
                    then start
                    else len + start

        toTake =
            clamper $
                if end >= 0
                    then end - toDrop
                    else len + end - toDrop

     in
        take toTake $ drop toDrop array


-- | > Determine if an array is empty.
-- | >
-- | >     isEmpty empty == true
-- |
-- | Equivalent to Purescript's `null`.
isEmpty :: ∀ a. Array a -> Bool
isEmpty = null
