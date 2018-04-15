
module Elm.Platform.Sub
  ( module Virtual
  , map
  , batch
  , none
  ) where


import Data.List (List(..))
import Elm.Platform (Sub) as Virtual
import Elm.Platform (Sub)
import Partial (crash)


map :: ∀ a msg. Partial => (a -> msg) -> Sub a -> Sub msg
map func sub =
    crash


batch :: ∀ msg. Partial => List (Sub msg) -> Sub msg
batch subs =
    crash


none :: ∀ msg. Partial => Sub msg
none =
    batch Nil
