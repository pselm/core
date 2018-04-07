
-- | Elm modules typically use `map2` through `map5` for what Purescript's
-- | `Apply` class would call `lift2` through `lift5`.
-- |
-- | So, we define `map2` through `map5` here as synonyms for `lift2` through
-- | `lift5`. We also re-export these in the individual Elm modules that use
-- | them, so that that the API matches with the Elm API.
-- |
-- | A few Elm modules use up to `map8`, so we implement those here as well.
-- |
-- | We also make `andMap` a synonym for Purescript's `apply`.

module Elm.Apply
    ( andMap
    , map2, map3, map4, map5, map6, map7, map8
    ) where


import Control.Apply (lift2, lift3, lift4, lift5)
import Prelude (class Apply, apply, (<$>), (<*>))


-- | Map a function of two arguments over some container type.
-- |
-- | The equivalent of Purescript's `lift2`.
map2 :: ∀ w a b c. (Apply w) => (a -> b -> c) -> w a -> w b -> w c
map2 = lift2


-- | Map a function of three arguments over some container type.
-- |
-- | The equivalent of Purescript's `lift3`.
map3 :: ∀ w a b c d. (Apply w) => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
map3 = lift3


-- | Map a function of four arguments over some container type.
-- |
-- | The equivalent of Purescript's `lift4`.
map4 :: ∀ w a b c d e. (Apply w) => (a -> b -> c -> d -> e) -> w a -> w b -> w c -> w d -> w e
map4 = lift4


-- | Map a function of five arguments over some container type.
-- |
-- | The equivalent of Purescript's `lift5`.
map5 :: ∀ w a b c d e f. (Apply w) => (a -> b -> c -> d -> e -> f) -> w a -> w b -> w c -> w d -> w e -> w f
map5 = lift5


-- | Map a function of six arguments over some container type.
map6 :: ∀ w a b c d e f g. (Apply w) => (a -> b -> c -> d -> e -> f -> g) -> w a -> w b -> w c -> w d -> w e -> w f -> w g
map6 func a b c d e f =
    func <$> a <*> b <*> c <*> d <*> e <*> f


-- | Map a function of seven arguments over some container type.
map7 :: ∀ w a b c d e f g h. (Apply w) => (a -> b -> c -> d -> e -> f -> g -> h) -> w a -> w b -> w c -> w d -> w e -> w f -> w g -> w h
map7 func a b c d e f g =
    func <$> a <*> b <*> c <*> d <*> e <*> f <*> g


-- | Map a function of eight arguments over some container type.
map8 :: ∀ w a b c d e f g h i. (Apply w) => (a -> b -> c -> d -> e -> f -> g -> h -> i) -> w a -> w b -> w c -> w d -> w e -> w f -> w g -> w h -> w i
map8 func a b c d e f g h =
    func <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h


-- | Map a function in a container to a value in a container.
-- |
-- | This is the equivalent of Purescript's `apply`.
andMap :: ∀ a b f. (Apply f) => f (a -> b) -> f a -> f b
andMap = apply
