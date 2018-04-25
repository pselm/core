
module Elm.Platform.Sub
  ( module Virtual
  ) where


import Elm.Monoid (batch, none) as Virtual
import Elm.Platform (Sub) as Virtual
import Prelude (map) as Virtual


-- Wasn't that easy? The real work here is going on in Elm.Platform, of course.
-- It's easier to define the `Sub` type there, in order to avoid cirucular
-- dependencies, and then re-export it here.
--
-- Plus, it turns out that everything else you can do with a `Sub` is actually
-- defined by its instances, so we don't have to do anything except re-export
-- stuff.
