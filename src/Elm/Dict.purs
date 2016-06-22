
-- | A dictionary mapping unique keys to values. The keys can be any type which
-- | has an instance of the `Ord` class.
-- |
-- | This is implemented in terms of Purescript's `Data.Map`.

module Elm.Dict
    ( module Virtual
    , Dict, get, remove, update
    , intersect, diff, filter, partition
    , map, foldl, foldr
    , toUnfoldable
    ) where


-- For re-export

import Data.Map
    ( empty, isEmpty, size
    , member
    , singleton, insert
    , toList, fromList, fromFoldable
    , keys, values
    , union
    ) as Virtual


-- Internal

import Prelude (class Ord, flip, (>>>))
import Prelude (map) as Prelude
import Data.Map (Map, lookup, alter, delete, member, insert, empty, toList, fromList)
import Data.Unfoldable (class Unfoldable)
import Data.Bifunctor (lmap, rmap)
import Data.Maybe (Maybe)
import Data.List (List)
import Data.List as List
import Elm.List as ElmList
import Elm.Basics (Bool)
import Data.Tuple (Tuple(..))


-- | Elm's `Dict` type is a synonym for Purescript's `Data.Map`.
type Dict = Map


-- TODO: It feels as though there ought to be a better implementation available
-- of various functions below, perhaps via bifunctor, profunctor or traversable?


-- | Get the value associated with a key. If the key is not found, return
-- | `Nothing`. This is useful when you are not sure if a key will be in the
-- | dictionary.
-- |
-- |     animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]
-- |
-- |     get "Tom"   animals == Just Cat
-- |     get "Jerry" animals == Just Mouse
-- |     get "Spike" animals == Nothing
-- |
-- | Equivalent to Purescript's `lookup`.
get :: ∀ k v. (Ord k) => k -> Dict k v -> Maybe v
get = lookup


-- | Remove a key-value pair from a dictionary. If the key is not found,
-- | no changes are made.
-- |
-- | Equivalent to Purescript's `delete`.
remove :: ∀ k v. (Ord k) => k -> Dict k v -> Dict k v
remove = delete


-- | Update the value of a dictionary for a specific key with a given function.
-- |
-- | Like Purescript's `alter`, but with flipped arguments.
update :: ∀ k v. (Ord k) => k -> (Maybe v -> Maybe v) -> Dict k v -> Dict k v
update = flip alter


-- | Keep a key-value pair when its key appears in the second dictionary.
-- | Preference is given to values in the first dictionary.
intersect :: ∀ k v. (Ord k) => Dict k v -> Dict k v -> Dict k v
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


-- | Keep a key-value pair when its key does not appear in the second dictionary.
diff :: ∀ k v. (Ord k) => Dict k v -> Dict k v -> Dict k v
diff t1 t2 =
    foldl (\k v t -> remove k t) t1 t2


-- | Keep a key-value pair when it satisfies a predicate.
filter :: ∀ k v. (Ord k) => (k -> v -> Bool) -> Dict k v -> Dict k v
filter predicate =
    let
        add key value dict =
            if predicate key value
                then insert key value dict
                else dict

    in
        foldl add empty


-- | Partition a dictionary according to a predicate. The first dictionary
-- | contains all key-value pairs which satisfy the predicate, and the second
-- | contains the rest.
partition :: ∀ k v. (Ord k) => (k -> v -> Bool) -> Dict k v -> Tuple (Dict k v) (Dict k v)
partition predicate =
    foldl add (Tuple empty empty)

    where
        add key value =
            (if predicate key value then lmap else rmap) (insert key value)


-- | Apply a function to all values in a dictionary.
map :: ∀ k a b. (Ord k) => (k -> a -> b) -> Dict k a -> Dict k b
map f dict =
    -- TODO: There has got to be a way to do this without constructing an
    -- intermediate list.
    let
        tuples :: List (Tuple k a)
        tuples = toList dict

        mapper :: Tuple k a -> Tuple k b
        mapper (Tuple k v) = Tuple k (f k v)

        mapped :: List (Tuple k b)
        mapped = Prelude.map mapper tuples

    in
        fromList mapped


-- | Fold over the key-value pairs in a dictionary, in order from lowest
-- | key to highest key.
foldl :: ∀ k v b. (Ord k) => (k -> v -> b -> b) -> b -> Dict k v -> b
foldl f acc dict =
    -- TODO: There has got to be a way to do this without constructing an
    -- intermediate list.
    let
        tuples :: List (Tuple k v)
        tuples = toList dict

        folder :: Tuple k v -> b -> b
        folder (Tuple k v) = f k v

    in
        ElmList.foldl folder acc tuples


-- | Fold over the key-value pairs in a dictionary, in order from highest
-- | key to lowest key.
foldr :: ∀ k v b. (Ord k) => (k -> v -> b -> b) -> b -> Dict k v -> b
foldr f acc dict =
    -- TODO: There has got to be a way to do this without constructing an
    -- intermediate list.
    let
        tuples :: List (Tuple k v)
        tuples = toList dict

        folder :: Tuple k v -> b -> b
        folder (Tuple k v) = f k v

    in
        ElmList.foldr folder acc tuples


-- | Produce a `Dict` from any `Unfoldable` container of tuples of keys
-- | and values. Defined polymorphically to accommodate Purescript `Array`,
-- | among others.
-- |
-- | Note that this is not in the Elm API.
toUnfoldable :: ∀ f k v. (Ord k, Unfoldable f) => Dict k v -> f (Tuple k v)
toUnfoldable =
    -- There is probably a more efficient method ...
    toList >>> List.toUnfoldable
