
-- | Some types broken out from other files to avoid
-- | circular dependencies.

module Elm.Types (Task, TaskE) where


import Control.Monad.Aff (Aff)
import Control.Monad.Except.Trans (ExceptT)


-- | Represents asynchronous effects that may fail. It is useful for stuff like
-- | HTTP.
-- | 
-- | For example, maybe we have a task with the type (`Task String User`). This means
-- | that when we perform the task, it will either fail with a `String` message or
-- | succeed with a `User`. So this could represent a task that is asking a server
-- | for a certain user.
-- |
-- | Implemented in terms of Purescript's `Aff` type, with `ExceptT` layered on top
-- | in order to provide for a polymorphically-typed error channel.
type Task x a = forall e. ExceptT x (Aff e) a


-- | Equivalent to a `Task`, but with the effect types specified.
type TaskE e x a = ExceptT x (Aff e) a
