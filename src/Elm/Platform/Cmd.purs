
-- | # Effects
-- |
-- | Elm has **managed effects**, meaning that things like HTTP requests or writing
-- | to disk are all treated as *data* in Elm. When this data is given to the Elm
-- | runtime system, it can do some “query optimization” before actually performing
-- | the effect. Perhaps unexpectedly, this managed effects idea is the heart of why
-- | Elm is so nice for testing, reuse, reproducibility, etc.
-- |
-- | There are two kinds of managed effects you will use in your programs: commands
-- | and subscriptions.

module Elm.Platform.Cmd
  ( module Virtual
  , withCmds
  , (!)
  ) where


import Data.Foldable (class Foldable, fold)
import Data.Monoid (class Monoid)
import Data.Tuple (Tuple(..))
import Elm.Monoid (batch, none) as Virtual
import Elm.Platform (Cmd) as Virtual
import Prelude ((<<<))
import Prelude (map) as Virtual


withCmds :: ∀ f model cmd. Foldable f => Monoid cmd => model -> f cmd -> Tuple model cmd
withCmds model =
    Tuple model <<< fold


infixl 5 withCmds as !


-- Wasn't that easy? The real work here is going on in Elm.Platform, of course.
-- It's easier to define the `Sub` type there, in order to avoid cirucular
-- dependencies, and then re-export it here.
--
-- And, several other functions Elm defines here are based on the instances for
-- Cmd, so they can just be re-exported as well.
