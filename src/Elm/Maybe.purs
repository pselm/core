
-- | > This library fills a bunch of important niches in Elm. A `Maybe` can help
-- | > you with optional arguments, error handling, and records with optional fields.
-- |
-- | This is implemented in terms of Purescript's `Data.Maybe`, so you can use functions
-- | from there on `Maybe` values as well.

module Elm.Maybe
    ( module Virtual
    , withDefault
    ) where


-- For re-export

import Data.Foldable (oneOf) as Virtual
import Data.Maybe (Maybe(..)) as Virtual
import Prelude (map) as Virtual
import Elm.Apply (map2, map3, map4, map5) as Virtual
import Elm.Bind (andThen) as Virtual

-- Internal

import Data.Maybe (Maybe, fromMaybe)


-- | > Provide a default value, turning an optional value into a normal
-- | > value.  This comes in handy when paired with functions like
-- | > [`Dict.get`](Dict#get) which gives back a `Maybe`.
-- | >
-- | >     withDefault 100 (Just 42)   -- 42
-- | >     withDefault 100 Nothing     -- 100
-- | >
-- | >     withDefault "unknown" (Dict.get "Tom" Dict.empty)   -- "unknown"
-- |
-- | Equivalent to Purescript's 'fromMaybe`.
withDefault :: ∀ a. a -> Maybe a -> a
withDefault = fromMaybe
