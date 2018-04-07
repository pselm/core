
-- | This library fills a bunch of important niches in Elm. A `Maybe` can help
-- | you with optional arguments, error handling, and records with optional fields.
-- |
-- | This is implemented in terms of Purescript's `Data.Maybe`, so you can use functions
-- | from there on `Maybe` values as well.

module Elm.Maybe
    ( module Virtual
    , withDefault, oneOf
    ) where


-- For re-export

import Data.Maybe (Maybe(..)) as Virtual
import Prelude (map) as Virtual
import Elm.Apply (map2, map3, map4, map5) as Virtual
import Elm.Bind (andThen) as Virtual

-- Internal

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (class Foldable, foldl)


-- | Provide a default value, turning an optional value into a normal
-- | value.  This comes in handy when paired with functions like
-- | [`Dict.get`](Dict#get) which gives back a `Maybe`.
-- |
-- |     withDefault 100 (Just 42)   -- 42
-- |     withDefault 100 Nothing     -- 100
-- |
-- |     withDefault "unknown" (Dict.get "Tom" Dict.empty)   -- "unknown"
-- |
-- | Equivalent to Purescript's 'fromMaybe`.
withDefault :: ∀ a. a -> Maybe a -> a
withDefault = fromMaybe


-- | Pick the first `Maybe` that actually has a value. Useful when you want to
-- | try a couple different things, but there is no default value.
-- |
-- |     oneOf [ Nothing, Just 42, Just 71 ] == Just 42
-- |     oneOf [ Nothing, Nothing, Just 71 ] == Just 71
-- |     oneOf [ Nothing, Nothing, Nothing ] == Nothing
-- |
-- | The signature uses `Foldable` to work with `List` or `Array`, among others
-- |
-- | This function was removed in Elm 0.18.
oneOf :: ∀ f a. (Foldable f) => f (Maybe a) -> Maybe a
oneOf =
    foldl stepOneOf Nothing
      where
        stepOneOf memo item =
            case memo of
                Nothing -> item
                Just _ -> memo
