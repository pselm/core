module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.IO (INFINITY)
import Prelude (Unit, discard)
import Test.Elm.Array as Array
import Test.Elm.Basics as Basics
import Test.Elm.BasicsElm as BasicsElm
import Test.Elm.Bitwise as Bitwise
import Test.Elm.Char as Char
import Test.Elm.Color as Color
import Test.Elm.Date as Date
import Test.Elm.Dict as Dict
import Test.Elm.Json as Json
import Test.Elm.List as List
import Test.Elm.ListElm as ListElm
import Test.Elm.Maybe as Maybe
import Test.Elm.Random as Random
import Test.Elm.Regex as Regex
import Test.Elm.Result as Result
import Test.Elm.Set as Set
import Test.Elm.String as String
import Test.Elm.Task as Task
import Test.Elm.Time as Time
import Test.Elm.Trampoline as Trampoline
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)


main :: Eff
    ( console :: CONSOLE
    , testOutput :: TESTOUTPUT
    , avar :: AVAR
    , exception :: EXCEPTION
    , infinity :: INFINITY
    , random :: RANDOM
    ) Unit
main =
    runTest do
       {- Array.tests
        Basics.tests
        BasicsElm.tests
        Bitwise.tests
        Char.tests
        Color.tests
        Date.tests
        Dict.tests -}
        Json.tests
    {- List.tests
        ListElm.tests
        Maybe.tests
        Random.tests
        Regex.tests
        Result.tests
        Set.tests
        String.tests
        Time.tests
        Task.tests
        Trampoline.tests -}
