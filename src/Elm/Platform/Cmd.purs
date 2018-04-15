
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
  , map
  , batch
  , none
  , withCmds
  , (!)
  ) where


import Data.List (List(..))
import Data.Tuple (Tuple(..))
import Elm.Platform (Cmd) as Virtual
import Elm.Platform (Cmd)
import Partial (crash)


map :: ∀ a msg. Partial => (a -> msg) -> Cmd a -> Cmd msg
map func cmd =
    crash


batch :: ∀ msg. Partial => List (Cmd msg) -> Cmd msg
batch cmds =
    crash


none :: ∀ msg. Partial => Cmd msg
none =
    batch Nil


withCmds :: ∀ model msg. Partial => model -> List (Cmd msg) -> Tuple model (Cmd msg)
withCmds model commands =
    Tuple model (batch commands)


infixl 5 withCmds as !
