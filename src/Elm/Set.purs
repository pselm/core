
-- | A set of unique values. The values can be any type with an
-- | `Ord` instance.
-- |
-- | This is implemented in terms of Purescript's `Data.Set`, so
-- | you can also use functions from that module on a `Set`.

module Elm.Set
    ( module Virtual
    , remove, intersect, diff
    , filter, partition, map
    ) where


-- For re-export

import Data.Set
    ( Set, empty, singleton, insert
    , isEmpty, member, size
    , toList, fromList
    , union
    ) as Virtual

import Data.Foldable (foldr) as Virtual
import Elm.Foldable (foldl) as Virtual


-- Internal

import Prelude (class Ord, (<<<))
import Data.Set (Set, delete, difference, intersection, fromList, toList, insert, empty)
import Data.Tuple (Tuple(..))
import Data.Bifunctor (lmap, rmap)
import Data.Foldable (foldr)
import Elm.Foldable (foldl)
import Elm.Basics (Bool)


-- | Remove a value from a set. If the value is not found, no changes are made.
-- |
-- | Equivalent to Purescript's `delete`.
remove :: ∀ a. (Ord a) => a -> Set a -> Set a
remove = delete


-- | Get the intersection of two sets. Keeps values that appear in both sets.
-- |
-- | Equivalent to Purescript's `intersection`.
intersect :: ∀ a. (Ord a) => Set a -> Set a -> Set a
intersect = intersection


-- | Get the difference between the first set and the second. Keeps values
-- | that do not appear in the second set.
-- |
-- | Equivalent to Purescript's `difference`.
diff :: ∀ a. (Ord a) => Set a -> Set a -> Set a
diff = difference


-- | Map a function onto a set, creating a new set with no duplicates.
map :: ∀ a b. (Ord a, Ord b) => (a -> b) -> Set a -> Set b
map func set =
    foldl (\a memo -> insert (func a) memo) empty set


-- | Create a new set consisting only of elements which satisfy a predicate.
filter :: ∀ a. (Ord a) => (a -> Bool) -> Set a -> Set a
filter func =
    fromList <<< Data.List.filter func <<< toList


-- | Create two new sets; the first consisting of elements which satisfy a
-- | predicate, the second consisting of elements which do not.
partition :: ∀ a. (Ord a) => (a -> Bool) -> Set a -> Tuple (Set a) (Set a)
partition pred =
    foldr step (Tuple empty empty)

    where
        step x =
            (if pred x then lmap else rmap) (insert x)
