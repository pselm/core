module Test.Elm.Trampoline (tests) where

import Test.Unit (TestSuite, suite, Test, test)
import Test.Unit.Assert (equal)

import Elm.Trampoline
import Prelude (class Show, class Eq, map, ($), bind, (+), (-), flip, zero, one, (==))
import Data.Foldable (sequence_)
import Elm.List ((..))
import Data.Int53 (Int53, truncate, fromInt)


infixl 9 equals as ===

equals :: ∀ a e. (Eq a, Show a) => a -> a -> Test e
equals = flip equal


badSum :: Int53 -> Int53
badSum n =
    let
        loop x acc =
            if x == zero
                then acc
                else loop (x - one) (acc + x)

    in
        loop n zero


goodSum :: Int53 -> Int53
goodSum n =
    let
        sumT x acc =
            if x == zero
                then Done acc
                else Continue (\_ -> sumT (x - one) (acc + x))

    in
        trampoline $ sumT n zero


mkLoopCheck :: ∀ e. Int -> Test e
mkLoopCheck n =
    (badSum $ fromInt n) === (goodSum $ fromInt n)


tests :: ∀ e. TestSuite e
tests = suite "Elm.Trampoline" do
    test "noStackOverflow" do
        (goodSum $ truncate 1000000.0) === truncate 500000500000.0

    test "loopChecks" $
        sequence_ $
            map mkLoopCheck (0 .. 25)

