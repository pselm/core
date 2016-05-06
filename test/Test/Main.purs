module Test.Main where

import Test.Unit (TIMER)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Aff.AVar (AVAR)

import Prelude (Unit, bind)

main :: ∀ e. Eff
    ( timer :: TIMER
    , avar :: AVAR
    , testOutput :: TESTOUTPUT
    , random :: RANDOM
    , err :: EXCEPTION
    , console :: CONSOLE
    | e) Unit

main = runTest do
    Test.Elm.Array.tests
    Test.Elm.Basics.tests
    Test.Elm.BasicsElm.tests
    Test.Elm.Bitwise.tests
    Test.Elm.Char.tests
    Test.Elm.Date.tests
    Test.Elm.Dict.tests
    Test.Elm.Json.tests
    Test.Elm.List.tests
    Test.Elm.ListElm.tests
    Test.Elm.Maybe.tests
    Test.Elm.Random.tests
    Test.Elm.Regex.tests
    Test.Elm.Result.tests
    Test.Elm.Set.tests
    Test.Elm.String.tests
    Test.Elm.Trampoline.tests
