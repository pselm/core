
-- | This module re-exports the things which Elm imports by default.
-- |
-- | So, if you want the Elm default imports, you can do
-- |
-- | `import Elm.Default`

module Elm.Default
    ( module Virtual
    , module Elm.Basics
    ) where


import Data.Maybe (Maybe (Just, Nothing)) as Virtual
import Data.List (List, (:)) as Virtual
import Data.Tuple (Tuple(..)) as Virtual
import Data.Tuple.Nested (type (/\), (/\)) as Virtual
import Elm.Basics
import Elm.Monoid (none) as Virtual
import Elm.Platform (Program) as Virtual
import Elm.Platform.Cmd (Cmd, (!)) as Virtual
import Elm.Platform.Sub (Sub) as Virtual
import Elm.Result (Result (Ok, Err)) as Virtual


-- These are Elm's default imports ...
--
-- <https://github.com/elm-lang/core>
--
-- import Basics exposing (..)
-- import List exposing (List, (::))
-- import Maybe exposing (Maybe(..))
-- import Result exposing (Result(..))
-- import String exposing (String)
-- import Char exposing (Char)
-- import Tuple
--
-- import Debug
--
-- import Platform exposing ( Program )
-- import Platform.Cmd as Cmd exposing ( Cmd )
-- import Platform.Sub as Sub exposing ( Sub )
