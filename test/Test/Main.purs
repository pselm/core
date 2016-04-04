module Test.Main where

import Test.Unit (TIMER, test, runTest)
import Test.Unit.Console (TESTOUTPUT)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Aff.AVar (AVAR)

import Prelude (Unit, ($), bind)

main :: ∀ e. Eff
    ( timer :: TIMER
    , avar :: AVAR
    , testOutput :: TESTOUTPUT
    , random :: RANDOM
    , err :: EXCEPTION
    , console :: CONSOLE
    | e) Unit
main = runTest do
    test "Elm.Array\n" $ Test.Elm.Array.tests
    test "Elm.Basics\n" $ Test.Elm.Basics.tests
    test "Elm.BasicsElm\n" $ Test.Elm.BasicsElm.tests
    test "Elm.Bitwise\n" $ Test.Elm.Bitwise.tests
    test "Elm.Char\n" $ Test.Elm.Char.tests
    test "Elm.Date\n" $ Test.Elm.Date.tests
    test "Elm.Dict\n" $ Test.Elm.Dict.tests
    test "Elm.Json\n" $ Test.Elm.Json.tests
    test "Elm.List\n" $ Test.Elm.List.tests
    test "Elm.ListElm\n" $ Test.Elm.ListElm.tests
    test "Elm.Maybe\n" $ Test.Elm.Maybe.tests
    test "Elm.Randon\n" $ Test.Elm.Random.tests
    test "Elm.Regex\n" $ Test.Elm.Regex.tests
    test "Elm.Result\n" $ Test.Elm.Result.tests
    test "Elm.Set\n" $ Test.Elm.Set.tests
    test "Elm.String\n" $ Test.Elm.String.tests
    test "Elm.Trampoline\n" $ Test.Elm.Trampoline.tests
