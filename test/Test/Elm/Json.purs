module Test.Elm.Json (tests) where

import Control.Alt (class Alt, alt, (<|>))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Plus (class Plus)
import Data.Array (toUnfoldable)
import Data.Foldable (traverse_)
import Data.Foreign (toForeign)
import Data.Generic (class Generic, gEq, gShow)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Sequence (Seq)
import Data.Sequence as Sequence
import Data.Tuple (Tuple(..))
import Elm.Basics (Float, (|>))
import Elm.Dict as Dict
import Elm.Json.Decode (Decoder, array, at, bool, customDecoder, dict, equalDecoders, fail, field, float, index, int, keyValuePairs, list, maybe, null, null_, nullable, object2, object3, oneOf, string, succeed, succeed_, tuple1, tuple2, tuple3, unfoldable, value, (:=))
import Elm.Json.Decode as JD
import Elm.Json.Encode as JE
import Elm.Result (Result(..), toMaybe)
import Math (sqrt)
import Prelude (class Applicative, class Apply, class Bind, class Eq, class Functor, class Monad, class Show, bind, discard, flip, map, negate, pure, show, ($), (+), (-), (<>), (==))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Laws.Control.Alt (checkAlt)
import Test.QuickCheck.Laws.Control.Applicative (checkApplicative)
import Test.QuickCheck.Laws.Control.Apply (checkApply)
import Test.QuickCheck.Laws.Control.Bind (checkBind)
import Test.QuickCheck.Laws.Control.Monad (checkMonad)
import Test.QuickCheck.Laws.Control.Plus (checkPlus)
import Test.QuickCheck.Laws.Data.Functor (checkFunctor)
import Test.Unit (TestSuite, Test, suite, test)
import Test.Unit.Assert (assert, assertFalse, equal)
import Type.Proxy (Proxy2(..))


infixl 9 equals as ===

equals :: ∀ a e. Eq a => Show a => a -> a -> Test e
equals = flip equal


infixr 0 tuple as ==>

tuple :: ∀ a b. a -> b -> Tuple a b
tuple = Tuple


check :: ∀ e a. Eq a => Show a => JD.Decoder a -> Tuple String (Maybe a) -> Test e
check decoder (Tuple value expected) =
    toMaybe (JD.decodeString decoder value) === expected


newtype Job = Job
    { name :: String
    , id :: Int
    , completed :: Boolean
    }

derive instance genericJob :: Generic Job

instance showJob :: Show Job where
    show = gShow

instance eqJob :: Eq Job where
    eq = gEq

makeJob :: String -> Int -> Boolean -> Job
makeJob a b c = Job $ {name: a, id: b, completed: c}


data Shape
    = Rectangle Float Float
    | Circle Float

derive instance genericShape :: Generic Shape

instance showShape :: Show Shape where
    show = gShow

instance eqShape :: Eq Shape where
    eq = gEq


-- TODO: The docs for Elm.Json.Encode.float say that infinity and NaN
-- are encoded as null ... should test that.

tests :: ∀ e. TestSuite (random :: RANDOM, exception :: EXCEPTION, console :: CONSOLE | e)
tests = suite "Json" do
    test "encode object with scalar types" do
        let
            person =
                JE.object
                    ( Tuple "name" (JE.string "Tom")
                    : Tuple "age" (JE.int 42)
                    : Tuple "height" (JE.float 234.1)
                    : Tuple "included" (JE.bool true)
                    : Tuple "member" (JE.bool false)
                    : Tuple "spouse" JE.null
                    : Nil
                    )

            compact =
                JE.encode 0 person

            readable =
                JE.encode 4 person

        compact === """{"name":"Tom","age":42,"height":234.1,"included":true,"member":false,"spouse":null}"""
        readable === """{
    "name": "Tom",
    "age": 42,
    "height": 234.1,
    "included": true,
    "member": false,
    "spouse": null
}"""

    test "encode object with scalar types (array arg)" do
        let
            person =
                JE.object
                    [ Tuple "name" (JE.string "Tom")
                    , Tuple "age" (JE.int 42)
                    , Tuple "height" (JE.float 234.1)
                    , Tuple "included" (JE.bool true)
                    , Tuple "member" (JE.bool false)
                    , Tuple "spouse" JE.null
                    ]

            compact =
                JE.encode 0 person

            readable =
                JE.encode 4 person

        compact === """{"name":"Tom","age":42,"height":234.1,"included":true,"member":false,"spouse":null}"""
        readable === """{
    "name": "Tom",
    "age": 42,
    "height": 234.1,
    "included": true,
    "member": false,
    "spouse": null
}"""

    test "encode arrays and lists" do
        let
            values :: Array JE.Value
            values = [ JE.string "Joe", JE.int 37 ]

            listOfValues :: List JE.Value
            listOfValues = List.fromFoldable values

            elmArray :: Seq JE.Value
            elmArray = Sequence.fromFoldable values

        JE.encode 0 (JE.array values) === """["Joe",37]"""
        JE.encode 0 (JE.array listOfValues) === """["Joe",37]"""
        JE.encode 0 (JE.array elmArray) === """["Joe",37]"""
        JE.encode 0 (JE.list listOfValues) === """["Joe",37]"""


    test "decode Int" $
        traverse_ (check JD.int)
            [ "4" ==> Just 4
            , "-4" ==> Just (-4)
            , "4.0" ==> Just 4
            , "-4.0" ==> Just (-4)

-- These are essentially Int53's ... need to figure this out
--            , "1801439850948"
--            , "-1801439850948"

            , "-4.2" ==> Nothing
            , "infinity" ==> Nothing
            , "-infinity" ==> Nothing
            , "nan" ==> Nothing
            , "-nan" ==> Nothing
            , "true" ==> Nothing
            , "false" ==> Nothing
            , "\"string\"" ==> Nothing
            , "{}" ==> Nothing
            , "null" ==> Nothing
            , "undefined" ==> Nothing
            ]

    test "decode String" $
        traverse_ (check JD.string)
            [ "4" ==> Nothing
            , "-4" ==> Nothing
            , "4.0" ==> Nothing
            , "-4.0" ==> Nothing
            , "1801439850948" ==> Nothing
            , "-1801439850948" ==> Nothing
            , "4.2" ==> Nothing
            , "-4.2" ==> Nothing
            , "infinity" ==> Nothing
            , "-infinity" ==> Nothing
            , "nan" ==> Nothing
            , "-nan" ==> Nothing
            , "true" ==> Nothing
            , "false" ==> Nothing
            , "\"string\"" ==> Just "string"
            , "{}" ==> Nothing
            , "null" ==> Nothing
            , "undefined" ==> Nothing
            ]


    test "decode Float" $
        traverse_ (check JD.float)
            [ "4" ==> Just 4.0
            , "-4" ==> Just (-4.0)
            , "4.0" ==> Just (4.0)
            , "-4.0" ==> Just (-4.0)
            , "1801439850948" ==> Just (1801439850948.0)
            , "-1801439850948" ==> Just (-1801439850948.0)
            , "4.2" ==> Just 4.2
            , "-4.2" ==> Just (-4.2)

-- TODO: Check these out ... they fail
--            , "infinity" ==> Just infinity
--            , "-infinity" ==> Just (-infinity)
--            , "nan" ==> Just (nan)
--            , "-nan" ==> Just (-nan)

            , "true" ==> Nothing
            , "false" ==> Nothing
            , "\"string\"" ==> Nothing
            , "{}" ==> Nothing
            , "null" ==> Nothing
            , "undefined" ==> Nothing
            ]


    test "decode bool" $
        traverse_ (check JD.bool)
            [ "4" ==> Nothing
            , "-4" ==> Nothing
            , "4.0" ==> Nothing
            , "-4.0" ==> Nothing
            , "1801439850948" ==> Nothing
            , "-1801439850948" ==> Nothing
            , "4.2" ==> Nothing
            , "-4.2" ==> Nothing
            , "infinity" ==> Nothing
            , "-infinity" ==> Nothing
            , "nan" ==> Nothing
            , "-nan" ==> Nothing
            , "true" ==> Just true
            , "false" ==> Just false
            , "\"string\"" ==> Nothing
            , "{}" ==> Nothing
            , "null" ==> Nothing
            , "undefined" ==> Nothing
            ]


    test "decode null" $
        traverse_ (check (JD.null "default"))
            [ "4" ==> Nothing
            , "-4" ==> Nothing
            , "4.0" ==> Nothing
            , "-4.0" ==> Nothing
            , "1801439850948" ==> Nothing
            , "-1801439850948" ==> Nothing
            , "4.2" ==> Nothing
            , "-4.2" ==> Nothing
            , "infinity" ==> Nothing
            , "-infinity" ==> Nothing
            , "nan" ==> Nothing
            , "-nan" ==> Nothing
            , "true" ==> Nothing
            , "false" ==> Nothing
            , "\"string\"" ==> Nothing
            , "{}" ==> Nothing
            , "null" ==> Just "default"
            , "undefined" ==> Nothing
            ]

    test "map" $
        traverse_ (check (JD.map (_ <> " Banana") JD.string))
            [ "\"Joe\"" ==> Just "Joe Banana"
            , "17" ==> Nothing
            ]

    test "object1" $
        traverse_ (check (JD.map (sqrt) JD.float))
            [ "\"Joe\"" ==> Nothing
            , "9" ==> Just 3.0
            ]

    test "(:=) and object2" do
        let
            decoder =
                JD.object2 Tuple
                    ("name" := JD.string)
                    ("age" := JD.int)

        traverse_ (check decoder)
            [ """ {"name": "Ryan", "age": 51} """ ==> Just (Tuple "Ryan" 51)
            , """ {"na "Ryan", "age": 51} """ ==> Nothing
            , """ {"name": "Ryan", "age": "51"} """ ==> Nothing
            , """ {"name": 52, "age": 51} """ ==> Nothing
            ]

    test "object3" do
        let
            decoder =
                JD.object3 makeJob
                    ("name" := JD.string)
                    ("id" := JD.int)
                    ("completed" := JD.bool)

        (check decoder)
            ( """ {"name": "Feed Cat", "id": 7, "completed": true} """
                ==> Just $ Job
                    { name: "Feed Cat"
                    , id: 7
                    , completed: true
                    }
            )

    test "at with List" do
        let
            decoder =
                JD.at ("target" : "value" : Nil) JD.string

        traverse_ (check decoder)
            [ """ {"target": {"value": "that"}} """ ==> Just "that"
            , """ {"target": "this", "value": "that"} """ ==> Nothing
            ]

    test "at with Array" do
        let
            decoder =
                JD.at ["target", "value"] JD.string

        traverse_ (check decoder)
            [ """ {"target": {"value": "that"}} """ ==> Just "that"
            , """ {"target": "this", "value": "that"} """ ==> Nothing
            ]

    test "keyValuePairs" do
        let
            decoder =
                JD.keyValuePairs JD.int

        traverse_ (check decoder)
            [ """ {"tom": 89, "sue": 92, "bill": 97} """
                ==> Just
                        ( Tuple "tom" 89
                        : Tuple "sue" 92
                        : Tuple "bill" 97
                        : Nil
                        )

            , """ {"tom": 89, "sue": "number", "bill": 97} """
                ==> Nothing
            ]

    test "keyValuePairs with Array" do
        let
            decoder =
                JD.keyValuePairs JD.int

        traverse_ (check decoder)
            [ """ {"tom": 89, "sue": 92, "bill": 97} """
                ==> Just
                        [ Tuple "tom" 89
                        , Tuple "sue" 92
                        , Tuple "bill" 97
                        ]

            , """ {"tom": 89, "sue": "number", "bill": 97} """
                ==> Nothing
            ]
    test "dict" do
        let
            decoder =
                JD.dict JD.int

        traverse_ (check decoder)
            [ """ {"tom": 89, "sue": 92, "bill": 97} """
                ==> Just $ Dict.fromList
                        ( Tuple "tom" 89
                        : Tuple "sue" 92
                        : Tuple "bill" 97
                        : Nil
                        )

            , """ {"tom": 89, "sue": "number", "bill": 97} """
                ==> Nothing
            ]


    test "alt" do
        let
            decoder =
                ("x" := JD.int) <|>
                ("y" := JD.int) <|>
                ("z" := JD.int)


        traverse_ (check $ JD.list decoder)
            [ """ [ {"x": 7, "y": 27}, {"y": 8}, {"z": 9} ] """
                ==> Just (7 : 8 : 9 : Nil)

            , """ [ {"x": 7, "y": 27}, {"y": "string"}, {"z": 9} ] """
                ==> Nothing
            ]


    test "oneOf" do
        let
            decoder =
                JD.oneOf
                    ( ("x" := JD.int)
                    : ("y" := JD.int)
                    : ("z" := JD.int)
                    : Nil
                    )

        traverse_ (check $ JD.list decoder)
            [ """ [ {"x": 7, "y": 27}, {"y": 8}, {"z": 9} ] """
                ==> Just (7 : 8 : 9 : Nil)

            , """ [ {"x": 7, "y": 27}, {"y": "string"}, {"z": 9} ] """
                ==> Nothing
            ]

    test "oneOf with array" do
        let
            decoder =
                JD.oneOf
                    [ ("x" := JD.int)
                    , ("y" := JD.int)
                    , ("z" := JD.int)
                    ]

        traverse_ (check $ JD.list decoder)
            [ """ [ {"x": 7, "y": 27}, {"y": 8}, {"z": 9} ] """
                ==> Just (7 : 8 : 9 : Nil)

            , """ [ {"x": 7, "y": 27}, {"y": "string"}, {"z": 9} ] """
                ==> Nothing
            ]

    test "list" do
        let decoder = list int
        traverse_ (check decoder)
            [ """ [ 7, 12, 13 ] """ ==> Just (7 : 12 : 13 : Nil)
            , """ 7 """ ==> Nothing
            , """ [ "12" ] """ ==> Nothing
            ]

    test "array" do
        let decoder = array int
        traverse_ (check decoder)
            [ """ [ 7, 12, 13 ] """ ==> Just (toUnfoldable [7, 12, 13])
            , """ 7 """ ==> Nothing
            , """ [ "12" ] """ ==> Nothing
            ]

    test "unfoldable" do
        let decoder = unfoldable int
        traverse_ (check decoder)
            [ """ [ 7, 12, 13 ] """ ==> Just [7, 12, 13]
            , """ 7 """ ==> Nothing
            , """ [ "12" ] """ ==> Nothing
            ]

    test "maybe" do
        let
            decoder =
                JD.maybe ("profession" := JD.string)

        traverse_ (check decoder)
            [ """ {"profession": "plumber"} """ ==> Just (Just "plumber")
            , """ {"age": 27} """ ==> Just (Nothing)
            ]


    test "decodeValue" do
        (JD.decodeValue JD.int (JE.int 27)) === Ok 27
        (JD.decodeValue JD.int (JE.string "bob")) === Err "(NonEmptyList (NonEmpty (TypeMismatch \"Int\" \"String\") Nil))"

    test "andThen" do
        let
            shapeInfo tag =
                case tag of
                    "rectangle" ->
                        JD.object2 Rectangle
                            ( "width" := JD.float )
                            ( "height" := JD.float )

                    "circle" ->
                        JD.object1 Circle
                            ( "radius" := JD.float )

                    _ ->
                        JD.fail (tag <> " is not a recoganized tag for shapes")

            decoder =
                ( "tag" := JD.string ) |> JD.andThen shapeInfo

        traverse_ (check decoder)
            [ """ {"tag": "rectangle", "width": 2.0, "height": 2.5} """ ==> Just (Rectangle 2.0 2.5)
            , """ {"tag": "circle", "radius": 14.0} """ ==> Just (Circle 14.0)
            , """ {"tag": "box"} """ ==> Nothing
            ]

    test "doNotation" do
        let
            shapeInfo tag =
                case tag of
                    "rectangle" ->
                        JD.object2 Rectangle
                            ( "width" := JD.float )
                            ( "height" := JD.float )

                    "circle" ->
                        JD.object1 Circle
                            ( "radius" := JD.float )

                    _ ->
                        JD.fail (tag <> " is not a recoganized tag for shapes")

            decoder = do
                tag <- "tag" := JD.string
                shapeInfo tag

        traverse_ (check decoder)
            [ """ {"tag": "rectangle", "width": 2.0, "height": 2.5} """ ==> Just (Rectangle 2.0 2.5)
            , """ {"tag": "circle", "radius": 14.0} """ ==> Just (Circle 14.0)
            , """ {"tag": "box"} """ ==> Nothing
            ]

    test "tuple1" do
        let
            decoder =
                JD.tuple1 show JD.int

        traverse_ (check decoder)
            [ "[17]" ==> Just "17"
            , "[17, 18]" ==> Nothing
            , "[]" ==> Nothing
            , "{}" ==> Nothing
            , "[17.5]" ==> Nothing
            ]


    test "tuple2" do
        let
            decoder =
                JD.tuple2 (+) JD.int JD.int

        traverse_ (check decoder)
            [ "[17]" ==> Nothing
            , "[17, 18]" ==> Just 35
            , "[17, 18, 19]" ==> Nothing
            , "[]" ==> Nothing
            , "{}" ==> Nothing
            , "[17.5, 18]" ==> Nothing
            ]

    test "tuple3" do
        let
            decoder =
                JD.tuple3 (\a b c -> a + b + c) JD.int JD.int JD.int

        traverse_ (check decoder)
            [ "[17]" ==> Nothing
            , "[17, 18]" ==> Nothing
            , "[17, 20, 30]" ==> Just 67
            , "[17, 20, 30, 32]" ==> Nothing
            , "[]" ==> Nothing
            , "{}" ==> Nothing
            , "[17.5, 18, 22]" ==> Nothing
            ]
    suite "equalDecoders" do
        test "succeed" do
            -- Should test something that actually relies on its `Eq` instance
            assert "equal" $ succeed 17 `equalDecoders` succeed 17
            assertFalse "unequal" $ succeed 17 `equalDecoders` succeed 18
            assertFalse "differnt" $ succeed 17 `equalDecoders` fail "problem"

        test "succeed_" do
            -- Should test something that actually relies on its `Eq` instance
            assert "equal" $ succeed_ 17 `equalDecoders` succeed_ 17
            assertFalse "unequal" $ succeed_ 17 `equalDecoders` succeed_ 18
            assertFalse "differnt" $ succeed_ 17 `equalDecoders` fail "problem"

        test "succeed & succeed_" do
            assert "equal" $ succeed 17 `equalDecoders` succeed_ 17
            assertFalse "unequal" $ succeed 17 `equalDecoders` succeed_ 18

        test "succeed_ & succeed" do
            assert "equal" $ succeed_ 17 `equalDecoders` succeed 17
            assertFalse "unequal" $ succeed_ 17 `equalDecoders` succeed 18

        test "null" do
            -- Should test something that actually relies on its `Eq` instance
            assert "equal" $ null 17 `equalDecoders` null 17
            assertFalse "unequal" $ null 17 `equalDecoders` null 18
            assertFalse "differnt" $ null 17 `equalDecoders` fail "problem"

        test "null_" do
            -- Should test something that actually relies on its `Eq` instance
            assert "equal" $ null_ 17 `equalDecoders` null_ 17
            assertFalse "unequal" $ null_ 17 `equalDecoders` null_ 18
            assertFalse "differnt" $ null_ 17 `equalDecoders` fail "problem"

        test "null & null_" do
            assert "equal" $ null 17 `equalDecoders` null_ 17
            assertFalse "unequal" $ null 17 `equalDecoders` null_ 18

        test "null_ & null" do
            assert "equal" $ null_ 17 `equalDecoders` null 17
            assertFalse "unequal" $ null_ 17 `equalDecoders` null 18

        test "fail" do
            assert "equal" $ fail "message" `equalDecoders` fail "message"
            assertFalse "unequal" $ fail "message" `equalDecoders` fail "other message"

        test "lazy" do
            let func = \_ -> succeed 17
            assert "same" $ JD.lazy func `equalDecoders` JD.lazy func
            -- assert "equal" $ JD.lazy (\_ -> succeed 17) `equalDecoders` JD.lazy (\_ -> succeed 17)
            assertFalse "unequal" $ JD.lazy (\_ -> succeed 17) `equalDecoders` JD.lazy (\_ -> succeed 18)

        test "value" do
            assert "equal" $ value `equalDecoders` value
            assertFalse "unequal" $ value `equalDecoders` succeed_ (toForeign 17)

        test "maybe" do
            assert "equal" $ maybe (succeed 17) `equalDecoders` maybe (succeed 17)
            assertFalse "unequal" $ maybe (succeed 17) `equalDecoders` maybe (succeed 18)

        test "nullable" do
            assert "equal" $ nullable (succeed 17) `equalDecoders` nullable (succeed 17)
            assertFalse "unequal" $ nullable (succeed 17) `equalDecoders` nullable (succeed 18)

        test "bool" do
            assert "equal" $ bool `equalDecoders` bool
            assertFalse "unequal" $ bool `equalDecoders` succeed true

        test "int" do
            assert "equal" $ int `equalDecoders` int
            assertFalse "unequal" $ int `equalDecoders` succeed 17

        test "float" do
            assert "equal" $ float `equalDecoders` float
            assertFalse "unequal" $ float `equalDecoders` succeed 17.0

        test "string" do
            assert "equal" $ string `equalDecoders` string
            assertFalse "unequal" $ string `equalDecoders` succeed "s"

        test "oneOf" do
            assert "equal 1" $ oneOf [ succeed 17, succeed 18 ] `equalDecoders` oneOf [ succeed 17, succeed 18 ]
            assert "equal 2" $ oneOf ( succeed 17 : succeed 18 : Nil ) `equalDecoders` oneOf [ succeed 17, succeed 18 ]
            assertFalse "unequal 1" $ oneOf [ succeed 17, succeed 18 ] `equalDecoders` oneOf [ succeed 17, succeed 19 ]
            assertFalse "unequal 2" $ oneOf [ succeed 17, succeed 18 ] `equalDecoders` oneOf [ succeed 17, succeed 18, succeed 19 ]

        test "keyValuePairs" do
            -- assert "equal" $ keyValuePairs int :: Decoder (List (Tuple String Int)) `equalDecoders` keyValuePairs int
            assertFalse "unequal" $ keyValuePairs int :: Decoder (List (Tuple String Int)) `equalDecoders` keyValuePairs (succeed 7)

        test "dict" do
            -- assert "equal" $ dict int `equalDecoders` dict int
            assertFalse "unequal" $ dict int `equalDecoders` dict (succeed 7)

        test "field" do
            assert "equal" $ field "age" int `equalDecoders` field "age" int
            assertFalse "unequal 1" $ field "age" int `equalDecoders` field "age" (succeed 7)
            assertFalse "unequal 2" $ field "age" int `equalDecoders` field "weight" int

        test "index" do
            assert "equal" $ index 0 int `equalDecoders` index 0 int
            assertFalse "unequal 1" $ index 0 int `equalDecoders` index 0 (succeed 7)
            assertFalse "unequal 2" $ index 0 int `equalDecoders` index 1 int

        test "at" do
            assert "equal 1" $ at [ "data", "age" ] int `equalDecoders` at [ "data", "age" ] int
            assert "equal 2" $ at ( "data" : "age" : Nil ) int `equalDecoders` at [ "data", "age" ] int
            assert "equal 3" $ at [ "data", "age" ] int `equalDecoders` field "data" (field "age" int)
            assertFalse "unequal 1" $ at [ "data", "age" ] int `equalDecoders` at [ "data", "weight" ] int
            assertFalse "unequal 2" $ at [ "data", "age" ] int `equalDecoders` at [ "data", "age", "inyears" ] int
            assertFalse "unequal 3" $ at [ "data", "age" ] int `equalDecoders` at [ "data", "age" ] (succeed 17)

        test "map" do
            let func = \x -> x + 1
            let func2 = \x -> x + 2
            assert "equal" $ map func (succeed 1) `equalDecoders` map func (succeed 1)
            assertFalse "unequal 1" $ map func (succeed 1) `equalDecoders` map func (succeed 2)
            assertFalse "unequal 2" $ map func (succeed 1) `equalDecoders` map func2 (succeed 1)

        test "alt" do
            assert "equal" $ alt (succeed 1) (succeed 2) `equalDecoders` alt (succeed 1) (succeed 2)
            assertFalse "unequal" $ alt (succeed 1) (succeed 2) `equalDecoders` alt (succeed 1) (succeed 3)

        test "bind" do
            let func = \x -> succeed (x + 1)
            let func2 = \x -> succeed (x + 2)
            assert "equal" $ bind (succeed 1) func `equalDecoders` bind (succeed 1) func
            assertFalse "unequal 1" $ bind (succeed 1) func `equalDecoders` bind (succeed 2) func
            assertFalse "unequal 2" $ bind (succeed 1) func `equalDecoders` bind (succeed 1) func2

        test "object2" do
            let func = \x y -> Tuple x y

            let decoderA1 = (field "name" string)
            let decoderB1 = (field "age" int)
            let decoderA2 = (field "name2" string)
            let decoderB2 = (field "age2" int)

            let decoder1 = JD.object2 Tuple ("name" := JD.string) ("age" := JD.int)
            let decoder2 = JD.object2 Tuple ("name" := JD.string) ("age" := JD.int)
            assert "equal" $ decoder1 `equalDecoders` decoder2

            -- However, should work if the decoders and func are referentially equal
            assert "equal" $ object2 Tuple decoderA1 decoderB1 `equalDecoders` object2 Tuple decoderA1 decoderB1
            assertFalse "unequal 1" $ object2 Tuple decoderA1 decoderB1 `equalDecoders` object2 Tuple decoderA1 decoderB2
            assertFalse "unequal 2" $ object2 Tuple decoderA1 decoderB1 `equalDecoders` object2 Tuple decoderA2 decoderB1
            assertFalse "unequal 3" $ object2 Tuple decoderA1 decoderB1 `equalDecoders` object2 func decoderA1 decoderB1

        test "object3" do
            let decoder1 = field "name" string
            let decoder2 = field "id" int
            let decoder3 = field "completed" bool
            assert "equal" $ object3 makeJob decoder1 decoder2 decoder3 `equalDecoders` object3 makeJob decoder1 decoder2 decoder3

            -- try separate production
            let decoderA = object3 makeJob (field "name" string) (field "id" int) (field "completed" bool)
            let decoderB = object3 makeJob (field "name" string) (field "id" int) (field "completed" bool)
            assert "separate" $ decoderA `equalDecoders` decoderB

        test "list" do
            assert "equal" $ list (succeed 17) `equalDecoders` list (succeed 17)
            assertFalse "unequal" $ list (succeed 17) `equalDecoders` list (succeed 18)

        test "array" do
            assert "equal" $ array (succeed 17) `equalDecoders` array (succeed 17)
            assertFalse "unequal" $ array (succeed 17) `equalDecoders` array (succeed 18)

        test "unfoldable" do
           assert "equal" $ unfoldable (succeed 17) :: Decoder (Array Int) `equalDecoders` unfoldable (succeed 17)
           assertFalse "unequal" $ unfoldable (succeed 17) :: Decoder (Array Int) `equalDecoders` unfoldable (succeed 18)

        test "tuple1" do
            let func = \x -> x + 1
            let func2 = \x -> x + 2
            assert "equal" $ tuple1 func (succeed 1) `equalDecoders` tuple1 func (succeed 1)
            assertFalse "unequal 1" $ tuple1 func (succeed 1) `equalDecoders` tuple1 func (succeed 2)
            assertFalse "unequal 2" $ tuple1 func (succeed 1) `equalDecoders` tuple1 func2 (succeed 1)

        test "tuple2" do
            let func = \a b -> a + b
            let func2 = \a b -> a - b
            assert "equal" $ tuple2 func (succeed 1) (succeed 2) `equalDecoders` tuple2 func (succeed 1) (succeed 2)
            assertFalse "unequal 1" $ tuple2 func (succeed 1) (succeed 2) `equalDecoders` tuple2 func (succeed 2) (succeed 2)
            assertFalse "unequal 2" $ tuple2 func (succeed 1) (succeed 2) `equalDecoders` tuple2 func2 (succeed 1) (succeed 2)

        test "tuple3" do
            let func = \a b c -> a + b + c
            let func2 = \a b c -> a - b - c
            assert "equal" $ tuple3 func (succeed 1) (succeed 2) (succeed 3) `equalDecoders` tuple3 func (succeed 1) (succeed 2) (succeed 3)
            assertFalse "unequal 1" $ tuple3 func (succeed 1) (succeed 2) (succeed 3) `equalDecoders` tuple3 func (succeed 2) (succeed 2) (succeed 3)
            assertFalse "unequal 2" $ tuple3 func (succeed 1) (succeed 2) (succeed 3) `equalDecoders` tuple3 func2 (succeed 1) (succeed 2) (succeed 3)

        test "customDecoder" do
            let func = \x -> Ok x
            let func2 = \x -> Err "error"
            assert "equal" $ customDecoder (succeed 1) func `equalDecoders` customDecoder (succeed 1) func
            assertFalse "unequal 1" $ customDecoder (succeed 1) func `equalDecoders` customDecoder (succeed 2) func
            assertFalse "unequal 2" $ customDecoder (succeed 1) func `equalDecoders` customDecoder (succeed 1) func2

    test "laws\n" $
        liftEff do
            checkFunctor proxyDecoder
            checkAlt proxyDecoder
            checkApply proxyDecoder
            checkApplicative proxyDecoder
            checkBind proxyDecoder
            checkMonad proxyDecoder
            checkPlus proxyDecoder


-- We test the laws via a newtype, in order to avoid making spurious
-- Eq and Arbitrary instances for JD.Decoder. (And, since we can't
-- do orphan instances).
newtype DecoderLaws a = DecoderLaws (JD.Decoder a)

derive newtype instance functorDecoderLaws :: Functor DecoderLaws
derive newtype instance altDecoderLaws :: Alt DecoderLaws
derive newtype instance applyDecoderLaws :: Apply DecoderLaws
derive newtype instance applicativeDecoderLaws :: Applicative DecoderLaws
derive newtype instance bindDecoderLaws :: Bind DecoderLaws
derive newtype instance plusDecoderLaws :: Plus DecoderLaws

instance monadDecoderLaws :: Monad DecoderLaws


-- This is even more arbitrary than usual ...
instance arbitraryDecoderLawsA :: (Arbitrary a) => Arbitrary (DecoderLaws a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary

        pure $ DecoderLaws
            if a
               then JD.succeed_ b
               else JD.fail c


-- Given that our arbitrary instance doesn't depend on what we're decoding, we
-- compare decoders by running them on null data. Hopefully this is still
-- testing something real, since the instances we're testing only depend on
-- the *composition* of the results.
--
-- Testing with `equalDecoders` as our `eq` doesn't work ... I think because
-- `equalDecoders` isn't really an `Eq` instance ... it's not reliable. So,
-- this is the best we can do.
instance eqDecoderLawsA :: (Eq a) => Eq (DecoderLaws a) where
    eq (DecoderLaws a) (DecoderLaws b) =
        JD.decodeValue a JE.null == JD.decodeValue b JE.null


proxyDecoder :: Proxy2 DecoderLaws
proxyDecoder = Proxy2
