
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
    ( trampoline
    , Trampoline(..)
    ) where


import Prelude (Unit, unit)


-- | A way to build computations that may be deeply recursive. We will take an
-- | example of a tail-recursive function and rewrite it in a way that lets us use
-- | a trampoline:
-- |
-- |     length :: List a -> Int
-- |     length list = length' 0 list
-- |
-- |     length' :: Int -> List a -> Int
-- |     length' accum list =
-- |         case list of
-- |           Nil        -> accum
-- |           Cons hd tl -> length' (accum + 1) tl
-- |
-- | This finds the length of a list, but if the list is too long, it may cause a
-- | stack overflow. We can rewrite it as follows:
-- |
-- |     length :: List a -> Int
-- |     length list = trampoline (length' 0 list)
-- |
-- |     length' :: Int -> List a -> Trampoline Int
-- |     length' accum list =
-- |         case list of
-- |           Nil        -> Done accum
-- |           Cons hd tl -> Continue (\() -> length' (accum + 1) tl)
-- |
-- | Now it uses a trampoline and can recurse without growing the stack!
data Trampoline a
    = Done a
    | Continue (Unit -> Trampoline a)


-- | Evaluate a trampolined value in constant space.
trampoline :: ∀ a. Trampoline a -> a
trampoline tramp =
    case tramp of
        Done value ->
            value

        Continue f ->
            trampoline (f unit)
