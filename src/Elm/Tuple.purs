
-- | Some helpers for working with 2-tuples.
-- |
-- | **Note:** For larger chunks of data, it is best to switch to using records. So
-- | instead of representing a 3D point as `(3,4,5)` and wondering why there are no
-- | helper functions, represent it as `{ x = 3, y = 4, z = 5 }` and use all the
-- | built-in syntax for records.
-- |
-- | This module was added in Elm 0.18.
-- |
-- | Implemented using Purescript's `Data.Tuple`

module Elm.Tuple
  ( first, second
  , mapFirst, mapSecond
  ) where


import Data.Tuple (Tuple, fst, snd)
import Data.Bifunctor (lmap)
import Prelude (map)


-- | Extract the first value from a tuple.
-- |
-- |     first (Tuple 3 4) == 3
-- |     first (Tuple "john" "doe") == "john"
-- |
-- | Equivalent to Purescript's `fst`
first :: ∀ a1 a2. Tuple a1 a2 -> a1
first = fst


-- | Extract the second value from a tuple.
-- |
-- |    second (Tuple 3 4) == 4
-- |    second (Tuple "john" "doe") == "doe"
-- |
-- | Equivalent to Purescript's `snd`
second :: ∀ a1 a2. Tuple a1 a2 -> a2
second = snd


-- | Transform the first value in a tuple.
-- |
-- |     import String
-- |
-- |     mapFirst String.reverse (Tuple "stressed" 16) == (Tuple "desserts" 16)
-- |     mapFirst String.length  (Tuple "stressed" 16) == (Tuple 8 16)
-- |
-- | Equivalent to Purescript's `lmap`
mapFirst :: ∀ a b a2. (a -> b) -> Tuple a a2 -> Tuple b a2
mapFirst = lmap


-- | Transform the second value in a tuple.
-- |
-- |     import String
-- |
-- |     mapSecond sqrt          ("stressed", 16) == ("stressed", 4)
-- |     mapSecond (\x -> x + 1) ("stressed", 16) == ("stressed", 17)
-- |
-- | Equivalent to Purescript's `map`
mapSecond :: ∀ a b a1. (a -> b) -> Tuple a1 a -> Tuple a1 b
mapSecond = map
