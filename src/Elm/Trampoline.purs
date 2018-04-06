
-- | A [trampoline](http://en.wikipedia.org/wiki/Tail-recursive_function#Through_trampolining)
-- | makes it possible to recursively call a function without growing the stack.
-- |
-- | Popular JavaScript implementations do not perform any tail-call elimination, so
-- | recursive functions can cause a stack overflow if they go too deep. Trampolines
-- | permit unbounded recursion despite limitations in JavaScript.
-- |
-- | This strategy may create many intermediate closures, which is very expensive in
-- | JavaScript, so use this library only when it is essential that you recurse deeply.
-- |
-- | Note that in Purescript, there is tail-call elimination, so you may not need this if you
-- | arrange your code to use tail-calls. If you do need a trampoline module, you could
-- | also consider [Control.Monad.Trampoline](https://pursuit.purescript.org/packages/purescript-free/0.9.1/docs/Control.Monad.Trampoline).
-- |
-- | * In Elm 0.17, this module was moved from core to its own package. *

module Elm.Trampoline
    ( Trampoline
    , done, jump
    , evaluate
    ) where

-- I tried implementing this in terms of Control.Monad.Trampoline, which worked ...
-- the tricky bit was:
--
--     jump = delay >>> join
--
-- But performance was much worse (probably due to the join), so I reverted to this.

import Prelude (Unit, unit)


-- | A computation that has been broken up into a bunch of smaller chunks. The
-- | programmer explicitly adds "pause points" so each chunk of computation can be
-- | run without making the stack any deeper.
data Trampoline a
    = Done a
    | Jump (Unit -> Trampoline a)


-- | When you do not want a computation to go through the trampoline.
done :: ∀ a. a -> Trampoline a
done = Done


-- | When you want a computation to be delayed so that it is handled by the
-- | trampoline.
jump :: ∀ a. (Unit -> Trampoline a) -> Trampoline a
jump = Jump


-- | Evaluate a trampolined value in constant space.
evaluate :: ∀ a. Trampoline a -> a
evaluate trampoline =
    case trampoline of
        Done value ->
            value

        Jump f ->
            evaluate (f unit)
