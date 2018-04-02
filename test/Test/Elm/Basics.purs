module Test.Elm.Basics (tests) where

import Test.Unit (TestSuite, Test, suite, test)
import Test.Unit.Assert (assert, equal)

import Elm.Basics
import Elm.List (List(..), (..), (:))
import Data.Int (even)
import Prelude (class Eq, class Show, discard, Ordering(..))


infixl 9 equals as ==>

equals :: ∀ a e. Eq a => Show a => a -> a -> Test e
equals = flip equal


tests :: ∀ e. TestSuite e
tests = suite "Basics" do
    test "(==)" do
        assert "true" <| 5 == 5
        assert "false" <| not <| 4 == 5

    test "(/=)" do
        assert "true" <| 4 /= 5
        assert "false" <| not <| 5 /= 5

    test "(<)" do
        assert "less" <| 4 < 5
        assert "greater" <| not <| 4 < 3
        assert "equal" <| not <| 4 < 4

    test "(<=)" do
        assert "less" <| 4 <= 5
        assert "greater" <| not <| 4 <= 3
        assert "equal" <| 4 <= 4

    test "(>)" do
        assert "less" <| not <| 4 > 5
        assert "greater" <| 4 > 3
        assert "equal" <| not <| 4 > 4

    test "(>=)" do
        assert "less" <| not <| 4 >= 5
        assert "greater" <| 4 >= 3
        assert "equal" <|  4 >= 4

    test "max" do
        assert "less" <| max 4 5 == 5
        assert "greater" <| max 5 4 == 5
        assert "equal" <| max 4 4 == 4

    test "min" do
        assert "less" <| min 4 5 == 4
        assert "greater" <| min 5 4 == 4
        assert "equal" <| min 4 4 == 4

    test "compare" do
        assert "less" <| compare 4 5 == LT
        assert "greater" <| compare 5 4 == GT
        assert "equal" <| compare 4 4 == EQ

    test "not" do
        assert "ture" <| not true == false
        assert "false" <| not false == true

    test "(&&)" do
        assert "true true" <| (true && true) == true
        assert "true false" <| (true && false) == false
        assert "false true" <| (false && true) == false
        assert "false false" <| (false && false) == false

    test "(||)" do
        assert "true true" <| (true || true) == true
        assert "true false" <| (true || false) == true
        assert "false true" <| (false || true) == true
        assert "false false" <| (false || false) == false

    test "xor" do
        assert "true true" <| (true `xor` true) == false
        assert "true false" <| (true `xor` false) == true
        assert "false true" <| (false `xor` true) == true
        assert "false false" <| (false `xor` false) == false

    test "(+)" do
        assert "int" <| 1 + 2 == 3
        assert "float" <| 1.0 + 2.0 == 3.0

    test "(-)" do
        assert "int" <| 3 - 1 == 2
        assert "float" <| 3.0 - 1.0 == 2.0

    test "(*)" do
        assert "int" <| 3 * 4 == 12
        assert "float" <| 3.0 * 4.0 == 12.0

    test "(/)" do
        assert "int" <| 9 / 3 == 3
        assert "float" <| 10.0 / 2.5 == 4.0

    test "(^)" do
        assert "int int" <| 2 ^ 3 == 8
        assert "float float" <| 2.0 ^ 3.0 == 8.0

    test "(//)" do
        assert "even" <| 9 // 3 == 3
        assert "remainder" <| 10 // 3 == 3

    test "rem" do
        assert "positive" <| 7 `rem` 2 == 1
        assert "negative " <| -1 `rem` 4 == -1

    test "(%)" do
        assert "positive" <| 7 % 2 == 1
        assert "negative " <| -1 % 4 == 3

    test "negate" do
        assert "positiveInt" <| negate 7 == -7
        assert "negativeInt" <| negate (-7) == 7
        assert "positiveFloat" <| negate 7.0 == -7.0
        assert "negativeFloat" <| negate (-7.0) == 7.0

    test "abs" do
        assert "positiveFloat" <| abs 7.0 == 7.0
        assert "negativeFloat" <| abs (-7.0) == 7.0
        assert "positiveInt" <| abs 7 == 7
        assert "negativeInt" <| abs (-7) == 7

    test "sqrt" do
        assert "sqrt" <| sqrt 9.0 == 3.0

    test "clamp" do
        assert "lowInt" <| clamp 100 200 50 == 100
        assert "midInt" <| clamp 100 200 150 == 150
        assert "highInt" <| clamp 100 200 250 == 200
        assert "lowFloat" <| clamp 100.0 200.0 50.0 == 100.0
        assert "midFloat" <| clamp 100.0 200.0 150.0 == 150.0
        assert "highFloat" <| clamp 100.0 200.0 250.0 == 200.0

    test "logBase" do
        assert "logBase10" <| logBase 10.0 100.0 == 2.0
        assert "logBase2" <| logBase 2.0 256.0 == 8.0

    test "e" do
        assert "e" <| e - 2.7 < 0.1

    test "pi" do
        assert "pi" <| pi - 3.14 < 0.1

    test "trig" do
        assert "cos" <| cos 1.0 < 1000.0
        assert "sin" <| sin 1.0 < 1000.0
        assert "tan" <| tan 1.0 < 1000.0
        assert "acos" <| acos 1.0 < 1000.0
        assert "asin" <| asin 1.0 < 1000.0
        assert "atan" <| atan 1.0 < 1000.0
        assert "atan2" <| atan2 1.0 1.0 < 1000.0

    test "round" do
        assert "round" <| round 1.5 == 2

    test "truncate" do
        assert "truncate" <| truncate 1.5 == 1

    test "floor" do
        assert "floor" <| floor 1.7 == 1

    test "ceiling" do
        assert "ceiling" <| ceiling 1.1 == 2

    test "toFloat" do
        assert "toFloat" <| toFloat 1 == 1.0

    test "radians" do
        assert "radians" <| radians 1.0 == 1.0

    test "degrees" do
        assert "degrees" <| degrees 180.0 == pi

    test "turns" do
        assert "turns" <| turns 0.5 == pi

    test "fromPolar" do
        let result = fromPolar (Tuple 9.0 (degrees 45.0))
        assert "fromPolar" <| (fst result) - 6.3 < 0.1 && (snd result) - 6.3 < 0.1

    test "toPolar" do
        let result = toPolar (Tuple 6.364 6.364)
        assert "toPolar" <| (fst result) - 8.9 < 0.2 && (snd result) - 44.5 < 1.0

    test "toString" do
        toString 42 ==> "42"
        toString (1 .. 2) ==> "(1 : 2 : Nil)"
        toString "he said, \"hi\"" ==> "\"he said, \\\"hi\\\"\""

    test "isNaN" do
        assert "div 0" <| isNaN (0.0 / 0.0)
        assert "negative sqrt" <| isNaN (sqrt (-1.0))
        assert "infinity" <| not <| isNaN (1.0 / 0.0)
        assert "1" <| not <| isNaN 1.0

    test "isInfinite" do
        assert "div 0" <| not <| isInfinite (0.0 / 0.0)
        assert "negative sqrt" <| not <| isInfinite (sqrt (-1.0))
        assert "infinity" <| isInfinite (1.0 / 0.0)
        assert "1" <| not <| isInfinite 1.0

    test "(++)" do
        assert "strings" <| "hello" ++ "world" == "helloworld"
        assert "lists" <| (1 : 1 : 2 : Nil) ++ (3 : 5 : 8 : Nil) == (1 : 1 : 2 : 3 : 5 : 8 : Nil)

    test "fst" do
        assert "fst" <| fst (Tuple 1 2) == 1

    test "snd" do
        assert "snd" <| snd (Tuple 1 2) == 2

    test "identity" do
        assert "identity" <| identity 5 == 5

    test "always" do
        assert "always" <| always 0 1 == 0

    test "(<|)" do
        assert "<|" <| not <| not <| true

    test "(>|)" do
        assert "|>" <| (true |> not |> not)

    test "(<<)" do
        let composed = not << even << floor << sqrt
        assert "true" <| composed 9.0
        assert "false" <| not composed 16.0

    test "(>>)" do
        let composed = sqrt >> floor >> even >> not
        assert "true" <| composed 9.0
        assert "false" <| not composed 16.0

    test "flip" do
        assert "flip" <| (flip (-)) 1 3 == 2

    test "curry" do
        let tupleFunc = \t -> (fst t) + (snd t)
        assert "curry" <| (curry tupleFunc) 2 3 == 5

    test "uncurry" do
        let curryFunc = \a b -> a + b
        assert "uncurry" <| (uncurry curryFunc) (Tuple 2 3) == 5
