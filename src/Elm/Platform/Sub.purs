
module Elm.Platform.Sub
  ( module Virtual
  , batch
  , none
  ) where


import Data.List (List(..))
import Elm.Platform (Sub) as Virtual
import Elm.Platform (Sub)
import Partial (crash)
import Prelude (map) as Virtual


batch :: ∀ msg. Partial => List (Sub msg) -> Sub msg
batch subs =
    crash


none :: ∀ msg. Partial => Sub msg
none =
    batch Nil
