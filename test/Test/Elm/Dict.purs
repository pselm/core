module Test.Elm.Dict (tests) where

import Test.Unit (TestSuite, Test, suite, test)
import Test.Unit.Assert (assert)

import Elm.Dict as Dict
import Elm.Dict (Dict)
import Elm.Basics (Tuple(..), (++), (<|), (|>), (==))
import Prelude (discard, class Eq, (<$>), map)
import Elm.List (List(..), (:), range)
import Elm.Maybe (Maybe(..))
import Data.Tuple (fst, snd)


infixl 9 tuple as :=

tuple :: ∀ a b. a -> b -> Tuple a b
tuple = Tuple


assertEqual :: ∀ a e. (Eq a) => String -> a -> a -> Test e
assertEqual name expected actual =
    assert name <| expected == actual


animals :: Dict String String
animals =
    Dict.fromList
        ( "Tom" := "cat"
        : "Jerry" := "mouse"
        : Nil
        )

animalsPlus :: Dict String String
animalsPlus =
    Dict.fromList
        ( "Tom" := "Tom: cat"
        : "Jerry" := "Jerry: mouse"
        : Nil
        )


tests :: ∀ e. TestSuite e
tests = suite "Dict" do
    test "build" do
        assertEqual "empty" (Dict.fromList (Nil :: List (Tuple String String))) (Dict.empty)
        assertEqual "singleton" (Dict.fromList ("k" := "v" : Nil)) (Dict.singleton "k" "v")
        assertEqual "insert" (Dict.fromList ("k" := "v" : Nil)) (Dict.insert "k" "v" Dict.empty)
        assertEqual "insert replace" (Dict.fromList ("k" := "vv" : Nil)) (Dict.insert "k" "vv" (Dict.singleton "k" "v"))
        assertEqual "update replace" (Dict.fromList ("k" := "vv" : Nil)) (Dict.update "k" (\v -> Just "vv") (Dict.singleton "k" "v"))
        assertEqual "update modify" (Dict.fromList ("k" := "vv" : Nil)) (Dict.update "k" ((<$>) ((++) "v")) (Dict.singleton "k" "v"))
        assertEqual "update new" (Dict.fromList ("k" := "v" : "y" := "vv" : Nil)) (Dict.update "y" (\v -> Just "vv") (Dict.singleton "k" "v"))
        assertEqual "update remove" Dict.empty (Dict.update "k" (\v -> Nothing) (Dict.singleton "k" "v"))
        assertEqual "remove" Dict.empty (Dict.remove "k" (Dict.singleton "k" "v"))
        assertEqual "remove not found" (Dict.singleton "k" "v") (Dict.remove "kk" (Dict.singleton "k" "v"))
        assertEqual "fromFoldable" (Dict.fromList ("k" := "v" : "k2" := "v2" : Nil)) (Dict.fromFoldable ["k" := "v", "k2" := "v2"])
        assertEqual "toUnfoldable" (Dict.toUnfoldable animals) [Tuple "Jerry" "mouse", Tuple "Tom" "cat"]

    test "query" do
        assertEqual "member 1" true (Dict.member "Tom" animals)
        assertEqual "member 2" false (Dict.member "Spike" animals)
        assertEqual "get 1" (Just "cat") (Dict.get "Tom" animals)
        assertEqual "get 2" (Nothing :: Maybe String) (Dict.get "Spike" animals)
        assertEqual "size of empty dictionary" 0 (Dict.size Dict.empty)
        assertEqual "size of example dictionary" 2 (Dict.size animals)
        assertEqual "isEmpty true" true (Dict.isEmpty Dict.empty)
        assertEqual "isEmpty false" false (Dict.isEmpty animals)

    test "combine" do
        assertEqual "union" animals (Dict.union (Dict.singleton "Jerry" "mouse") (Dict.singleton "Tom" "cat"))
        assertEqual "union collison" (Dict.singleton "Tom" "cat") (Dict.union (Dict.singleton "Tom" "cat") (Dict.singleton "Tom" "mouse"))
        assertEqual "intersect" (Dict.singleton "Tom" "cat") (Dict.intersect animals (Dict.singleton "Tom" "cat"))
        assertEqual "diff" (Dict.singleton "Jerry" "mouse") (Dict.diff animals (Dict.singleton "Tom" "cat"))

    test "transform" do
        assertEqual "filter" (Dict.singleton "Tom" "cat") (Dict.filter (\k v -> k == "Tom") animals)

        let partitioned = Dict.partition (\k v -> k == "Tom") animals
        assertEqual "partition trues" (Dict.singleton "Tom" "cat") (fst partitioned)
        assertEqual "partition falses" (Dict.singleton "Jerry" "mouse") (snd partitioned)

        assertEqual "keys" ("Jerry" : "Tom" : Nil) (Dict.keys animals)
        assertEqual "values" ("mouse" : "cat" : Nil) (Dict.values animals)

        assertEqual "map" animalsPlus <|
            Dict.map (\k v -> k ++ ": " ++ v) animals

        assertEqual "foldl" ("Tom: cat" : "Jerry: mouse" : Nil) <|
            Dict.foldl (\k v b -> (k ++ ": " ++ v) : b) Nil animals

        assertEqual "foldr" ("Jerry: mouse" : "Tom: cat" : Nil) <|
            Dict.foldr (\k v b -> (k ++ ": " ++ v) : b) Nil animals

    test "merge" do
        assertEqual "merge empties" (Dict.empty) <|
            (Dict.merge Dict.insert insertBoth Dict.insert Dict.empty Dict.empty Dict.empty)

        assertEqual "merge singletons in order" [Tuple "u1" [1], Tuple "u2" [2]] <|
            ((Dict.merge Dict.insert insertBoth Dict.insert s1 s2 Dict.empty) |> Dict.toList)

        assertEqual "merge singletons out of order" [Tuple "u1" [1], Tuple "u2" [2]] <|
            ((Dict.merge Dict.insert insertBoth Dict.insert s2 s1 Dict.empty) |> Dict.toList)

        assertEqual "merge with duplicate key" [Tuple "u2" [2, 3]] <|
            ((Dict.merge Dict.insert insertBoth Dict.insert s2 s23 Dict.empty) |> Dict.toList)

        assertEqual "partially overlapping" bExpected <|
            ((Dict.merge Dict.insert insertBoth2 Dict.insert b1 b2 Dict.empty) |> Dict.toList)

        where
            insertBoth key leftVal rightVal dict = Dict.insert key (leftVal ++ rightVal) dict
            insertBoth2 key leftVal rightVal dict = Dict.insert key (leftVal ++ rightVal) dict

            s1 = Dict.empty |> Dict.insert "u1" [ 1 ]
            s2 = Dict.empty |> Dict.insert "u2" [ 2 ]
            s23 = Dict.empty |> Dict.insert "u2" [ 3 ]

            b1 = map (\i -> Tuple i [i]) (range 1 10) |> Dict.fromList
            b2 = map (\i -> Tuple i [i]) (range 5 15) |> Dict.fromList

            bExpected = [Tuple 1 [1], Tuple 2 [2], Tuple 3 [3], Tuple 4 [4], Tuple 5 [5,5], Tuple 6 [6,6], Tuple 7 [7,7], Tuple 8 [8,8], Tuple 9 [9,9], Tuple 10 [10,10], Tuple 11 [11], Tuple 12 [12], Tuple 13 [13], Tuple 14 [14], Tuple 15 [15]]
