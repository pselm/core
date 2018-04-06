module Test.Elm.Random (tests) where

import Test.Unit (TestSuite, Test, suite, test, success, failure)
import Test.Unit.Assert (assert, equal)

import Elm.Random
import Prelude (bind, discard, (<), (-), (+), ($), (<>), show, negate, map)
import Elm.Basics ((<|), (==), abs)
import Data.Int53 (Int53, fromInt)
import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))


close :: ∀ e. Number -> Number -> Test e
close expected actual =
    if abs (expected - actual) < 0.00000001
        then success
        else failure $ "expected " <> show expected <> ", got " <> show actual


infixl 9 close as ~=


seed1 :: Seed
seed1 = initialSeed 1283428


seed2 :: Seed
seed2 = initialSeed 8224729


seed3 :: Seed
seed3 = initialSeed 547283


genInt53 :: Generator Int53
genInt53 = int (fromInt 0) (fromInt 100)


genInt :: Generator Int
genInt = int 0 100


genFloat :: Generator Number
genFloat = float 0.0 100.0


doer :: Generator Int
doer = do
    y <- int 0 100
    int 0 y


tests :: ∀ e. TestSuite e
tests = suite "Random" do
    test "bool" do
        assert "seed1" <| false == (step bool seed1).value
        assert "seed2" <| false == (step bool seed2).value
        assert "seed3" <| true == (step bool seed3).value

    test "int53" do
        assert "seed1" <| fromInt 12 == (step genInt53 seed1).value
        assert "seed2" <| fromInt 31 == (step genInt53 seed2).value
        assert "seed3" <| fromInt 40 == (step genInt53 seed3).value

    test "int" do
        assert "seed1" <| 12 == (step genInt seed1).value
        assert "seed2" <| 31 == (step genInt seed2).value
        assert "seed3" <| 40 == (step genInt seed3).value

    suite "float" do
        test "seed1" <| 17.00685023 ~= (step genFloat seed1).value
        test "seed2" <| 63.96765709 ~= (step genFloat seed2).value
        test "seed3" <| 44.89097595 ~= (step genFloat seed3).value

    test "pair" do
        assert "seed1" <| Tuple false true == (step (pair bool bool) seed1).value
        assert "seed2" <| Tuple false false == (step (pair bool bool) seed2).value
        assert "seed3" <| Tuple true false == (step (pair bool bool) seed3).value

    test "list" do
        assert "seed1" <| (fromFoldable [12,46,21,81,68,39,0,78,73,69]) == (step (list 10 genInt) seed1).value
        assert "seed2" <| (fromFoldable [31,54,62,45,91,27,15,38,83,3]) == (step (list 10 genInt) seed2).value
        assert "seed3" <| (fromFoldable [40,52,34,44,69,14,2,28,48,29]) == (step (list 10 genInt) seed3).value

    test "array" do
        assert "seed1" <| [12,46,21,81,68,39,0,78,73,69] == (step (list 10 genInt) seed1).value
        assert "seed2" <| [31,54,62,45,91,27,15,38,83,3] == (step (list 10 genInt) seed2).value
        assert "seed3" <| [40,52,34,44,69,14,2,28,48,29] == (step (list 10 genInt) seed3).value

    test "Ramdom.map" do
        assert "map"  <| 13 == (step (map ((+) 1) genInt) seed1).value
        assert "map2" <| (-34) == (step (map2 (-) genInt genInt) seed1).value
        assert "map3" <| 37 == (step (map3 (\a b c -> a + b - c  ) genInt genInt genInt) seed1).value
        assert "map4" <| 68 == (step (map4 (\a b c d -> a - b + c + d) genInt genInt genInt genInt) seed1).value
        assert "map5" <| 186 == (step (map5 (\a b c d e -> a + b - c + d + e) genInt genInt genInt genInt genInt) seed1).value

    test "andThen" do
        let genAndThen = genInt `andThen` int 0
        assert "seed1" <| 12 == (step genAndThen seed1).value
        assert "seed2" <| 12 == (step genAndThen seed2).value
        assert "seed3" <| 40 == (step genAndThen seed3).value

    test "do notation" do
        assert "seed1" <| 12 == (step doer seed1).value
        assert "seed2" <| 12 == (step doer seed2).value
        assert "seed3" <| 40 == (step doer seed3).value

    test "minInt" $
        equal "(Int53 -2147483648.0)" (show minInt)

    test "maxInt" $
        equal "(Int53 2147483647.0)" (show maxInt)
