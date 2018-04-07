module Test.Elm.Json (tests) where

import Test.Unit (TestSuite, Test, suite, test)
import Test.Unit.Assert (equal)

import Elm.Json.Encode as JE
import Elm.Json.Decode as JD
import Elm.Json.Decode ((:=))

import Elm.Result (Result(..), toMaybe)
import Elm.Basics (Float, (|>))
import Elm.Dict as Dict

import Data.List (List(..), (:))
import Data.List as List
import Data.Foldable (traverse_)
import Data.Sequence (Seq)
import Data.Sequence as Sequence
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Generic (class Generic, gEq, gShow)

import Math (sqrt)

import Control.Alt (class Alt, alt, (<|>))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE)

import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Laws.Data.Functor (checkFunctor)
import Test.QuickCheck.Laws.Control.Alt (checkAlt)
import Test.QuickCheck.Laws.Control.Apply (checkApply)
import Test.QuickCheck.Laws.Control.Applicative (checkApplicative)
import Test.QuickCheck.Laws.Control.Bind (checkBind)
import Test.QuickCheck.Laws.Control.Monad (checkMonad)
import Type.Proxy (Proxy2(..))

import Prelude
    ( class Show, show
    , class Monad, class Bind, bind
    , class Eq, (==)
    , class Functor, map
    , class Apply, apply
    , class Applicative, pure
    , flip, negate, discard, (<>), ($), (+), (<<<)
    )


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
            , "[]" ==> Nothing
            , "{}" ==> Nothing
            , "[17.5, 18]" ==> Nothing
            ]

    test "laws\n" $
        liftEff do
            checkFunctor proxyDecoder
            checkAlt proxyDecoder
            checkApply proxyDecoder
            checkApplicative proxyDecoder
            checkBind proxyDecoder
            checkMonad proxyDecoder


-- We test the laws via a newtype, in order to avoid making spurious
-- Eq and Arbitrary instances for JD.Decoder. (And, since we can't
-- do orphan instances).
newtype DecoderLaws a = DecoderLaws (JD.Decoder a)


instance functorDecoderLaws :: Functor DecoderLaws where
    map func (DecoderLaws decoder) = DecoderLaws $ map func decoder


instance altDecoderLaws :: Alt DecoderLaws where
    alt (DecoderLaws left) (DecoderLaws right) = DecoderLaws $ alt left right


instance applyDecoderLaws :: Apply DecoderLaws where
    apply (DecoderLaws func) (DecoderLaws decoder) = DecoderLaws $ apply func decoder


instance applicativeDecoderLaws :: Applicative DecoderLaws where
    pure = DecoderLaws <<< pure


instance bindDecoderLaws :: Bind DecoderLaws where
    bind (DecoderLaws decoder) func =
        DecoderLaws $ bind decoder
            \a -> case func a of
                DecoderLaws x -> x


instance monadDecoderLaws :: Monad DecoderLaws


-- This is even more arbitrary than usual ...
instance arbitraryDecoderLawsA :: (Arbitrary a) => Arbitrary (DecoderLaws a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary

        pure $ DecoderLaws
            if a
               then JD.succeed b
               else JD.fail c


-- Given that our arbitrary instance doesn't depend on what we're decoding, we
-- compare decoders by running them on null data. Hopefully this is still
-- testing something real, since the instances we're testing only depend on
-- the *composition* of the results.
instance eqDecoderLawsA :: (Eq a) => Eq (DecoderLaws a) where
    eq (DecoderLaws a) (DecoderLaws b) =
        JD.decodeValue a JE.null == JD.decodeValue b JE.null


proxyDecoder :: Proxy2 DecoderLaws
proxyDecoder = Proxy2
