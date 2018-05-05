
-- | Elm modules sometimes define `indexedMap` as a synonym for Purescript's
-- | `mapWithIndex`. So, we define that generally here.

module Elm.FunctorWithIndex (indexedMap) where


import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)


-- | Map over a container with an index.
-- |
-- | Equivalent to Purescript's `mapWithIndex`
indexedMap :: ∀ i f a b. FunctorWithIndex i f => (i -> a -> b) -> f a -> f b
indexedMap = mapWithIndex
