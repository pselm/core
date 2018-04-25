
-- | Elm modules define `foldl` with a different signature than Purescript's.
-- | So, we define that alternative `foldl` here.

module Elm.Foldable (foldl) where


import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Prelude (flip, (<<<))


-- | Reduce a container from the left.
-- |
-- | Equivalent to Purescript's `foldl`, but the function you supply is flipped.
foldl :: ∀ a b f. (Foldable f) => (a -> b -> b) -> b -> f a -> b
foldl = Foldable.foldl <<< flip
