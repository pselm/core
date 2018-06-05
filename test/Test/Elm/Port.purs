module Test.Elm.Port (tests) where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Elm.Json.Encode (encode)
import Elm.Port (toPort)
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
