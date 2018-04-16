
-- | Elm modules sometimes define a `none` function that corresponds to Purescript's
-- | `mempty`, and a `batch` function that corresponds to Purescript's `fold`. So,
-- | we re-export these here with the Elm-ish names.

module Elm.Monoid (batch, none) where


import Data.Foldable (class Foldable, fold)
import Data.Monoid (class Monoid, mempty)


-- | Produce an "empty" value of the relevant type.
-- |
-- | Equivalent to Purescript's `mempty`.
none :: ∀ m. Monoid m => m
none = mempty


-- | Smush together a bunch of things.
-- |
-- | Equivalent to Purescript's `fold`
batch :: ∀ f m. Foldable f => Monoid m => f m -> m
batch = fold
