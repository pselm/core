
-- | Elm modules sometimes use `repeat` where Purescript would use unfoldable's
-- | `replicate`. So, we define `repeat` as a synonym for `replicate` here.

module Elm.Unfoldable
    ( repeat
    ) where


import Data.Unfoldable (class Unfoldable, replicate)

-- | Repeat a value some natural number of times. For example:
-- |
-- |     replicate 2 "foo" == ("foo" : "foo" : Nil ) :: List String
-- |
-- | Equivalent to Purescript's `replicate`
repeat :: ∀ f a. Unfoldable f => Int -> a -> f a
repeat = replicate
