module Test.Elm.Port (tests) where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Elm.Json.Encode (encode)
import Elm.Port (fromPort, toPort)
import Elm.Result (Result(..))
import Prelude (class Eq, class Show, discard, flip, ($))
import Test.Unit (TestSuite, Test, suite, test)
import Test.Unit.Assert (equal)


infixl 9 equals as ===

equals :: ∀ a e. Eq a => Show a => a -> a -> Test e
equals = flip equal


tests :: ∀ e. TestSuite e
tests =
    suite "Port" do
        suite "encode" do
            test "boolean" do
                encode 0 (toPort false) === "false"
                encode 0 (toPort true) === "true"

            test "string" do
                encode 0 (toPort "a string") === "\"a string\""

            test "int" do
                encode 0 (toPort 17) === "17"

            test "float" do
                encode 0 (toPort 0.25) === "0.25"

            test "list" do
                encode 0 (toPort (1 : 2 : 5 : Nil)) === "[1,2,5]"

            test "Array" do
                encode 0 (toPort [1, 2, 6]) === "[1,2,6]"

            test "Maybe" do
                encode 0 (toPort (Nothing :: Maybe Int)) === "null"
                encode 0 (toPort $ Just 17) === "17"

            test "tuples" do
                encode 0 (toPort $ 17 /\ 18) === "[17,18]"
                encode 0 (toPort $ 17 /\ 18 /\ 19) === "[17,18,19]"
                encode 0 (toPort $ 17 /\ 18 /\ 19 /\ 20) === "[17,18,19,20]"

        suite "decode" do
            -- TODO: Use quickcheck
            test "boolean" do
                fromPort (toPort false) === Ok false
                fromPort (toPort true) === Ok true

            test "string" do
                fromPort (toPort "a string") === Ok "a string"

            test "int" do
                fromPort (toPort 17) === Ok 17

            test "float" do
                fromPort (toPort 0.25) === Ok 0.25

            test "list" do
                fromPort (toPort [1, 2, 5]) === Ok (1 : 2 : 5 : Nil)

            test "Array" do
                fromPort (toPort [1, 2, 6]) === Ok [1, 2, 6]

            test "Maybe" do
                fromPort (toPort (Nothing :: Maybe Int)) === Ok (Nothing :: Maybe Int)
                fromPort (toPort 17) === Ok (Just 17)

            test "tuples" do
                fromPort (toPort [17, 18]) === Ok (17 /\ 18)
                fromPort (toPort [17, 18, 19]) === Ok (17 /\ 18 /\ 19)
                fromPort (toPort [17, 18, 19, 20]) === Ok (17 /\ 18 /\ 19 /\ 20)
