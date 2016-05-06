module Test.Elm.List (tests) where

import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)

import Elm.List

import Elm.Basics (sqrt, (+), compare, (<|), (==), not, (<), (&&))
import Elm.Maybe (Maybe(..))
import Data.Int (even)
import Data.Tuple (Tuple(..))
import Prelude (bind, Ordering(..))


tests :: ∀ e. TestSuite e
tests = suite "Elm.List" do
    test "(:)" do
        assert "with rest" <| (1 : 2 : 3 : Nil) == (Cons 1 (Cons 2 (Cons 3 Nil)))
        assert "by itself" <| (1 : Nil) == (Cons 1 Nil)

    test "head" do
        assert "full" <| head (1 : 2 : 3 : Nil) == Just 1
        assert "empty" <| head Nil == (Nothing :: Maybe (List Int))

    test "tail" do
        assert "full" <| tail (1 : 2 : 3 : Nil) == (Just (2 : 3 : Nil))
        assert "empty" <| tail Nil == (Nothing :: Maybe (List Int))

    test "isEmpty" do
        assert "full" <| not <| isEmpty (1 : Nil)
        assert "empty" <| isEmpty Nil

    test "member" do
        assert "absent" <| not <| member 9 (1 : 2 : 3 : 4 : Nil)
        assert "present" <| member 4 (1 : 2 : 3 : 4 : Nil)

    test "map" do
        assert "sqrt" <| map sqrt (1.0 : 4.0 : 9.0 : Nil) == (1.0 : 2.0 : 3.0 : Nil)
        assert "not" <| map not (true : false : true : Nil) == (false : true : false : Nil)

    test "foldl" do
        assert "foldl" <| foldl (:) Nil (1 : 2 : 3 : Nil) == (3 : 2 : 1 : Nil)
        assert "foldl sum" <| foldl (+) 0 (1 : 2 : 3 : Nil) == 6

    test "foldr" do
        assert "foldr" <| foldr (:) Nil (1 : 2 : 3 : Nil) == (1 : 2 : 3 : Nil)
        assert "foldr sum" <| foldl (+) 0 (1 : 2 : 3 : Nil) == 6

    test "filter" do
        assert "filter" <| filter even (1 : 2 : 3 : 4 : 5 : 6: Nil) == (2 : 4 : 6 : Nil)

    test "length" do
        assert "length" <| length (1 : 2 : 3 : Nil) == 3

    test "reverse" do
        assert "reverse" <| reverse (1 : 2 : 3 : 4 : Nil) == (4 : 3 : 2 : 1 : Nil)

    test "append" do
        assert "append" <| append (1 : 1 : 2 : Nil) (3 : 5 : 8 : Nil) == (1 : 1 : 2 : 3 : 5 : 8 : Nil)
        assert "append char" <| append ("a" : "b" : Nil) ("c" : Nil) == ("a" : "b" : "c" : Nil)

    test "concat" do
        assert "concat" <| concat ((1 : 2 : Nil) : (3 : Nil) : (4 : 5 : Nil) : Nil) == (1 : 2 : 3 : 4 : 5 : Nil)

    test "concatMap" do
        assert "concat" <| concatMap (\x -> (x : Nil)) (1 : 2 : Nil) == (1 : 2 : Nil)

    test "sum" do
        assert "sum" <| sum (1 : 2 : 3 : 4 : Nil) == 10

    test "all" do
        assert "allTrue" <| all even (2 : 4 : Nil)
        assert "allFalse" <| not <| all even (2 : 3 : Nil)
        assert "allEmpty" <| all even Nil

    test "any" do
        assert "anyTrue" <| any even (2 : 3 : Nil)
        assert "anyFalse" <| not <| any even (1 : 3 : Nil)
        assert "anyEmpty" <| not <| any even Nil

    test "maximum" do
        assert "something" <| maximum (1 : 4 : 2 : Nil) == Just 4
        assert "nothing" <| maximum (Nil :: List Int) == (Nothing :: Maybe Int)

    test "minimum" do
        assert "something" <| minimum (3 : 2 : 1 : Nil) == Just 1
        assert "nothing" <| minimum (Nil :: List Int) == (Nothing :: Maybe Int)

    test "product" do
        assert "product" <| product (1 : 2 : 3 : 4 : Nil) == 24

    test "scanl" do
        assert "scanl" <| scanl (+) 0 (1 : 2 : 3 : 4 : Nil) == (0 : 1 : 3 : 6 : 10 : Nil)

    test "indexedMap" do
        assert "indexedMap" <| indexedMap Tuple ("Tom" : "Sue" : "Bob" : Nil) == (Tuple 0 "Tom" : Tuple 1 "Sue" : Tuple 2 "Bob" : Nil)

    test "filterMap" do
        assert "filterMap" <| filterMap Data.Int.fromString ("3" : "4.1" : "5" : "hats" : Nil) == (3 : 5 : Nil)

    test "map2" do
        assert "2nd longer" <| map2 (+) (1 .. 3) (1 .. 4) == (2 : 4 : 6 : Nil)
        assert "1st longer" <| map2 Tuple (1 .. 3) ('a' : 'b' : Nil) == (Tuple 1 'a' : Tuple 2 'b' : Nil)

    test "map3" do
        assert "map3" <| map3 func3 (1 .. 3) (1 .. 4) (1 .. 4) == (3 : 6 : 9 : Nil)

    test "map4" do
        assert "map4" <| map4 func4 (1 .. 3) (1 .. 4) (1 .. 4) (1 .. 4) == (4 : 8 : 12 : Nil)

    test "map5" do
        assert "map5" <| map5 func5 (1 .. 3) (1 .. 4) (1 .. 4) (1 .. 4) (1 .. 4) == (5 : 10 : 15 : Nil)

    test "partition" do
        let result = partition (\x -> x < 3) (0 .. 5)
        assert "partition" <| result.trues == (0 : 1 : 2 : Nil) && result.falses == (3 : 4 : 5 : Nil)

    test "unzip" do
        assert "unzip" <| unzip (Tuple 0 true : Tuple 17 false : Tuple 1337 true : Nil ) == Tuple (0 : 17 : 1337 : Nil) (true : false : true : Nil)

    test "intersperse" do
        assert "intersperse" <| intersperse "on" ("turtles1" : "turtles2" : "turtles3" : Nil) == ("turtles1" : "on" : "turtles2" : "on" : "turtles3" : Nil)

    test "take" do
        assert "take" <| take 2 (1 .. 4) == (1 .. 2)

    test "drop" do
        assert "drop" <| drop 2 (1 .. 4) == (3 .. 4)

    test "repeat" do
        assert "repeat" <| repeat 3 "repeat" == ("repeat" : "repeat" : "repeat" : Nil)

    test "sort" do
        assert "sort" <| sort (3 : 1 : 5 : Nil) == (1 : 3 : 5 : Nil)

    test "sortBy" do
        let
            alice = { name: "Alice", height: 1.62 }
            bob   = { name: "Bob"  , height: 1.85 }
            chuck = { name: "Chuck", height: 1.76 }

        assert "sortByName" <| map _.name (sortBy _.name (chuck : alice : bob : Nil)) == (alice.name : bob.name : chuck.name : Nil)
        assert "sortByHeight" <| map _.height (sortBy _.height (chuck : alice : bob : Nil)) == (alice.height : chuck.height : bob.height : Nil)
        assert "sortByLength" <| sortBy Data.String.length ("mouse" : "cat" : Nil) == ("cat" : "mouse" : Nil)

    test "sortWith" do
        assert "sortWith" <| sortWith flippedComparison (1 .. 5) == (5 : 4 : 3 : 2 : 1 : Nil)

    test "(..)" do
        assert "ascending" <| (1 .. 5) == (1 : 2 : 3 : 4 : 5 : Nil)
        assert "descending" <| (5 .. 1) == (Nil :: List Int)

    where
        func3 a b c =
            a + b + c

        func4 a b c d =
            a + b + c + d

        func5 a b c d e =
            a + b + c + d + e

        flippedComparison a b =
            case compare a b of
                LT -> GT
                EQ -> EQ
                GT -> LT
