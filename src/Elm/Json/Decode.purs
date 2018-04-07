
-- | Turn JSON values into Elm values. Definitely check out this [intro to
-- | JSON decoders][guide] to get a feel for how this library works!
-- |
-- | [guide]: https://guide.elm-lang.org/interop/json.html
-- |
-- | Elm's `Json.Decode` doesn't seem to be quite like any existing Purescript
-- | package, so I've re-implemented it, using parts of purescript-foreign as
-- | a base. For other approaches to decoding JSON in Purescript, you could see
-- | purescript-foreign, and the purescript-argonaut-* packages.

module Elm.Json.Decode
    ( module Virtual
    , module Elm.Bind
    , module Elm.Apply
    , Decoder
    , decodeString, decodeValue
    , fromForeign
    , string, int, float, bool, null
    , list, array, unfoldable
    , tuple1, tuple2, tuple3, tuple4, tuple5, tuple6, tuple7, tuple8
    , field, (:=), at, index
    , object1, object2, object3, object4, object5, object6, object7, object8
    , keyValuePairs, dict
    , oneOf, nullable, maybe, fail, succeed
    , value, customDecoder, lazy
    ) where


import Prelude (map) as Virtual
import Elm.Json.Encode (Value) as Virtual
import Elm.Bind (andThen)
import Elm.Apply (andMap, map2, map3, map4, map5, map6, map7, map8)

import Data.Foreign (Foreign, ForeignError(..), F, readArray, readString, readBoolean, readNumber, readInt, isNull, isUndefined, typeOf)
import Data.Foreign as DF
import Data.Foreign.Index (readIndex, readProp)
import Data.Foreign.JSON (parseJSON)
import Data.Traversable (traverse)
import Data.Monoid (class Monoid)
import Data.Foldable (class Foldable, foldl, foldMap)
import Data.Unfoldable (class Unfoldable, unfoldr)
import Elm.Json.Encode (Value)
import Control.Apply (lift2, lift3, lift4, lift5)
import Control.Alt (class Alt, alt)
import Control.Bind ((>=>))
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Elm.Result (Result(..), fromMaybe)
import Elm.Basics (Bool, Float)
import Elm.List (List, foldr)
import Data.List as List
import Elm.Dict (Dict)
import Elm.Dict as Dict
import Data.Tuple (Tuple(..))
import Elm.Maybe (Maybe(..))
import Data.Array (uncons)
import Data.Array as Array

import Prelude
    ( class Functor, map
    , class Apply, apply
    , class Applicative, pure
    , class Bind, bind
    , class Monad, (>>=)
    , Unit, unit, show, ($), (<<<), (<$>), const, (==), (<>)
    )


-- | A value that knows how to decode JSON values.
newtype Decoder a = Decoder (Value -> Result String a)


-- The Foreign module uses `Except` to pass errors around,
-- whereas we need `Result String a`.
toResult :: ∀ a. F a -> Result String a
toResult f =
    case runExcept f of
        Right a -> Ok a
        Left err -> Err (show err)


instance functorDecoder :: Functor Decoder where
    map func (Decoder decoder) =
        Decoder $ map func <<< decoder


instance altDecoder :: Alt Decoder where
    alt (Decoder left) (Decoder right) =
        Decoder $ \val ->
            case left val of
                Ok result ->
                    Ok result

                Err leftErr ->
                    case right val of
                        Ok result ->
                            Ok result

                        Err rightErr ->
                            Err $ leftErr <> " <|> " <> rightErr


instance applyDecoder :: Apply Decoder where
    apply (Decoder func) (Decoder decoder) =
        Decoder $ \val ->
            apply (func val) (decoder val)


instance applicativeDecoder :: Applicative Decoder where
    pure = Decoder <<< const <<< pure


instance bindDecoder :: Bind Decoder where
    bind (Decoder decoder) func =
        Decoder $ \val ->
            case decoder val of
                Err err ->
                    Err err

                Ok a ->
                    decodeValue (func a) val


instance monadDecoder :: Monad Decoder


-- | Parse the given string into a JSON value and then run the `Decoder` on it.
-- | This will fail if the string is not well-formed JSON or if the `Decoder`
-- | fails for some reason.
-- |
-- |     decodeString int "4"     == Ok 4
-- |     decodeString int "1 + 2" == Err ...
decodeString :: ∀ a. Decoder a -> String -> Result String a
decodeString (Decoder decoder) str =
    toResult (parseJSON str) >>= decoder


-- OBJECTS

-- | Decode a nested JSON object, requiring certain fields.
-- |
-- |     json = """{ "person": { "name": "tom", "age": 42 } }"""
-- |
-- |     decodeString (at ["person", "name"] string) json  == Ok "tom"
-- |     decodeString (at ["person", "age" ] int   ) json  == Ok "42
-- |
-- | This is really just a shorthand for saying things like:
-- |
-- |     field "person" (field "name" string) == at ["person","name"] string
-- |
-- | Note that the signature is defined in terms of `Foldable` so that it will
-- | work with `Array` or `List` (among others).
at :: ∀ f a. (Foldable f) => f String -> Decoder a -> Decoder a
at fields decoder =
    foldr field decoder fields


-- | Decode a JSON array, requiring a particular index.
-- |
-- |     json = """[ "alice", "bob", "chuck" ]"""
-- |
-- |     decodeString (index 0 string) json  == Ok "alice"
-- |     decodeString (index 1 string) json  == Ok "bob"
-- |     decodeString (index 2 string) json  == Ok "chuck"
-- |     decodeString (index 3 string) json  == Err ...
-- |
-- | This function was added in Elm 0.18.
index :: ∀ a. Int -> Decoder a -> Decoder a
index i (Decoder decoder) =
    Decoder $ \val ->
        toResult (readIndex i val) >>= decoder


-- | Decode a JSON object, requiring a particular field.
-- |
-- |     decodeString (field "x" int) "{ \"x\": 3 }"            == Ok 3
-- |     decodeString (field "x" int) "{ \"x\": 3, \"y\": 4 }"  == Ok 3
-- |     decodeString (field "x" int) "{ \"x\": true }"         == Err ...
-- |     decodeString (field "x" int) "{ \"y\": 4 }"            == Err ...
-- |
-- |     decodeString (field "name" string) "{ \"name\": \"tom\" }" == Ok "tom"
-- |
-- | The object *can* have other fields. Lots of them! The only thing this decoder
-- | cares about is if `x` is present and that the value there is an `Int`.
-- |
-- | Check out [`map2`](#map2) to see how to decode multiple fields!
field :: ∀ a. String -> Decoder a -> Decoder a
field f (Decoder decoder) =
    Decoder $ \val ->
        toResult (readProp f val) >>= decoder

infixl 4 field as :=


-- | Apply a function to a decoder.
-- |
-- |     object1 sqrt ("x" := float)
-- |
-- | Equivalent to Purescript's `map`.
-- |
-- | Removed in Elm 0.18, in favour of `map`.
object1 :: ∀ a value. (a -> value) -> Decoder a -> Decoder value
object1 = map


-- | Use two different decoders on a JS value. This is nice for extracting
-- | multiple fields from an object.
-- |
-- |     point :: Decoder (Tuple Float Float)
-- |     point =
-- |         object2 Tuple
-- |           ("x" := float)
-- |           ("y" := float)
-- |
-- | Equivalent to Purescript's `lift2`.
-- |
-- | Removed in Elm 0.18, in favour of `map2`.
object2 :: ∀ a b value. (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
object2 = lift2


-- | Use three different decoders on a JS value. This is nice for extracting
-- | multiple fields from an object.
-- |
-- |     type Job = { name :: String, id :: Int, completed :: Bool }
-- |
-- |     job :: Decoder Job
-- |     job =
-- |         object3 Job
-- |           ("name" := string)
-- |           ("id" := int)
-- |           ("completed" := bool)
-- |
-- | Equivalent to Purescript's `lift3`.
-- |
-- | Removed in Elm 0.18, in favour of `map3`.
object3 :: ∀ a b c value. (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
object3 = lift3


-- | Equivalent to Purescript's `lift4`.
-- |
-- | Removed in Elm 0.18, in favour of `map4`.
object4 :: ∀ a b c d value. (a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder value
object4 = lift4


-- | Equivalent to Purescript's `lift5`.
-- |
-- | Removed in Elm 0.18, in favour of `map5`.
object5 :: ∀ a b c d e value. (a -> b -> c -> d -> e -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder value
object5 = lift5


-- | Removed in Elm 0.18, in favour of `map6`.
object6 :: ∀ a b c d e f value. (a -> b -> c -> d -> e -> f -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder value
object6 = map6


-- | Removed in Elm 0.18, in favour of `map7`.
object7 :: ∀ a b c d e f g value. (a -> b -> c -> d -> e -> f -> g -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder value
object7 = map7


-- | Removed in Elm 0.18, in favour of `map8`.
object8 :: ∀ a b c d e f g h value. (a -> b -> c -> d -> e -> f -> g -> h -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder value
object8 = map8


-- | Decode a JSON object into an Elm `List` of pairs.
-- |
-- |     decodeString (keyValuePairs int) "{ \"alice\": 42, \"bob\": 99 }"
-- |       == [("alice", 42), ("bob", 99)]
-- |
-- | The container for the return type is polymorphic in order to accommodate `List` or `Array`, among others.
keyValuePairs :: ∀ f a. Monoid (f (Tuple String a)) => Applicative f => Decoder a -> Decoder (f (Tuple String a))
keyValuePairs (Decoder decoder) =
    Decoder $ \val -> do
        ks <- keys val
        arr <- traverse (\key -> do
            p <- toResult $ readProp key val
            d <- decoder p
            pure $ Tuple key d
        ) ks
        pure $ foldMap pure arr


-- | Get an array of the keys defined on a foreign value.
-- |
-- | This is from Data.Foreign.Keys, except that it used hasOwnProperty, whereas
-- | the Elm version also includes inherited properties.
keys :: Foreign -> Result String (Array String)
keys val | isNull val = toResult $ DF.fail $ TypeMismatch "object" "null"
keys val | isUndefined val = toResult $ DF.fail $ TypeMismatch "object" "undefined"
keys val | typeOf val == "object" = Ok $ unsafeKeys val
keys val = toResult $ DF.fail $ TypeMismatch "object" (typeOf val)

-- | From Data.Foreign, except includes inherited properties (as the Elm equivalent does)
foreign import unsafeKeys :: Foreign -> Array String


-- | Decode a JSON object into an Elm `Dict`.
-- |
-- |     decodeString (dict int) "{ \"alice\": 42, \"bob\": 99 }"
-- |       == Dict.fromList [("alice", 42), ("bob", 99)]
dict :: ∀ a. Decoder a -> Decoder (Dict String a)
dict decoder =
    let
        decodePairs :: Decoder (List (Tuple String a))
        decodePairs =
            keyValuePairs decoder
    in
    map Dict.fromList decodePairs


-- | Try a bunch of different decoders. This can be useful if the JSON may come
-- | in a couple different formats. For example, say you want to read an array of
-- | numbers, but some of them are `null`.
-- |
-- |     import String
-- |
-- |     badInt : Decoder Int
-- |     badInt =
-- |       oneOf [ int, null 0 ]
-- |
-- |     -- decodeString (list badInt) "[1,2,null,4]" == Ok [1,2,0,4]
-- |
-- | Why would someone generate JSON like this? Questions like this are not good
-- | for your health. The point is that you can use `oneOf` to handle situations
-- | like this!
-- |
-- | You could also use `oneOf` to help version your data. Try the latest format,
-- | then a few older ones that you still support. You could use `andThen` to be
-- | even more particular if you wanted.
-- |
-- | The container has a polymorphic type to accommodate `List` or `Array`,
-- | among others.
oneOf :: ∀ f a. (Foldable f) => f (Decoder a) -> Decoder a
oneOf decoders =
    foldl alt (fail "Expected one of: ") decoders


-- | Given a function which reads a `Foreign`, make a decoder.
-- |
-- | Note that this is not in the Elm API.
fromForeign :: ∀ a. (Foreign -> F a) ->  Decoder a
fromForeign func = Decoder $ toResult <<< func


-- | Decode a JSON string into an Elm `String`.
-- |
-- |     decodeString string "true"              == Err ...
-- |     decodeString string "42"                == Err ...
-- |     decodeString string "3.14"              == Err ...
-- |     decodeString string "\"hello\""         == Ok "hello"
-- |     decodeString string "{ \"hello\": 42 }" == Err ...
string :: Decoder String
string = fromForeign readString


-- | Decode a JSON number into an Elm `Float`.
-- |
-- |     decodeString float "true"              == Err ..
-- |     decodeString float "42"                == Ok 42
-- |     decodeString float "3.14"              == Ok 3.14
-- |     decodeString float "\"hello\""         == Err ...
-- |     decodeString float "{ \"hello\": 42 }" == Err ...
float :: Decoder Float
float = fromForeign readNumber


-- | Decode a JSON number into an Elm `Int`.
-- |
-- |     decodeString int "true"              == Err ...
-- |     decodeString int "42"                == Ok 42
-- |     decodeString int "3.14"              == Err ...
-- |     decodeString int "\"hello\""         == Err ...
-- |     decodeString int "{ \"hello\": 42 }" == Err ...
int :: Decoder Int
int = fromForeign readInt


-- | Decode a JSON boolean into an Elm `Bool`.
-- |
-- |     decodeString bool "true"              == Ok True
-- |     decodeString bool "42"                == Err ...
-- |     decodeString bool "3.14"              == Err ...
-- |     decodeString bool "\"hello\""         == Err ...
-- |     decodeString bool "{ \"hello\": 42 }" == Err ...
bool :: Decoder Bool
bool = fromForeign readBoolean


-- | Decode a JSON array into an Elm `List`.
-- |
-- |     decodeString (list int) "[1,2,3]"       == Ok [1,2,3]
-- |     decodeString (list bool) "[true,false]" == Ok [True,False]
list :: ∀ a. Decoder a -> Decoder (List a)
list (Decoder decoder) =
    Decoder $ \val -> do
        arr <- toResult $ readArray val
        List.fromFoldable <$> traverse decoder arr


-- | Decode a JSON array into an Elm `Array`.
-- |
-- |     decodeString (array int) "[1,2,3]"       == Ok (Array.fromList [1,2,3])
-- |     decodeString (array bool) "[true,false]" == Ok (Array.fromList [True,False])
-- |
-- | The return type is polymorphic to accommodate `Array` and `Elm.Array`,
-- | among others.
array :: ∀ f a. (Unfoldable f) => Decoder a -> Decoder (f a)
array = unfoldable


-- | Extract any `Unfoldable` from a JS array.
-- |
-- |     -- [1,2,3,4]
-- |
-- |     numbers :: Decoder (Array Int)
-- |     numbers =
-- |         unfoldable int
-- |
-- | Note that this is not part of the Elm API.
unfoldable :: ∀ f a. (Unfoldable f) => Decoder a -> Decoder (f a)
unfoldable (Decoder decoder) =
    Decoder $ \val -> do
        arr <- toResult $ readArray val
        decoded <- traverse decoder arr
        pure $ unfoldr
            (\a -> map (\b -> Tuple b.head b.tail) (uncons a))
            decoded


-- | Decode a `null` value into some Elm value.
-- |
-- |     decodeString (null False) "null" == Ok False
-- |     decodeString (null 42) "null"    == Ok 42
-- |     decodeString (null 42) "42"      == Err ..
-- |     decodeString (null 42) "false"   == Err ..
-- |
-- | So if you ever see a `null`, this will return whatever value you specified.
null :: ∀ a. a -> Decoder a
null default =
    Decoder $
        \val ->
            if isNull val
                then Ok default
                else toResult $ DF.fail $ TypeMismatch "null" (typeOf val)


-- | Decode a nullable JSON value into an Elm value.
-- |
-- |     decodeString (nullable int) "13"    == Ok (Just 13)
-- |     decodeString (nullable int) "42"    == Ok (Just 42)
-- |     decodeString (nullable int) "null"  == Ok Nothing
-- |     decodeString (nullable int) "true"  == Err ..
-- |
-- | This function was added in Elm 0.18.
nullable :: ∀ a. Decoder a -> Decoder (Maybe a)
nullable decoder =
    oneOf
        [ null Nothing
        , map Just decoder
        ]


-- | Helpful for dealing with optional fields. Here are a few slightly different
-- | examples:
-- |
-- |     json = """{ "name": "tom", "age": 42 }"""
-- |
-- |     decodeString (maybe (field "age"    int  )) json == Ok (Just 42)
-- |     decodeString (maybe (field "name"   int  )) json == Ok Nothing
-- |     decodeString (maybe (field "height" float)) json == Ok Nothing
-- |
-- |     decodeString (field "age"    (maybe int  )) json == Ok (Just 42)
-- |     decodeString (field "name"   (maybe int  )) json == Ok Nothing
-- |     decodeString (field "height" (maybe float)) json == Err ...
-- |
-- | Notice the last example! It is saying we *must* have a field named `height` and
-- | the content *may* be a float. There is no `height` field, so the decoder fails.
-- |
-- | Point is, `maybe` will make exactly what it contains conditional. For optional
-- | fields, this means you probably want it *outside* a use of `field` or `at`.
maybe :: ∀ a. Decoder a -> Decoder (Maybe a)
maybe (Decoder decoder) =
    Decoder $ \val ->
        case decoder val of
            Ok decoded ->
                Ok (Just decoded)

            Err _ ->
                Ok Nothing


-- | Do not do anything with a JSON value, just bring it into Elm as a `Value`.
-- | This can be useful if you have particularly crazy data that you would like to
-- | deal with later. Or if you are going to send it out a port and do not care
-- | about its structure.
value :: Decoder Value
value = Decoder $ Ok


-- | Run a `Decoder` on some JSON `Value`. You can send these JSON values
-- | through ports, so that is probably the main time you would use this function.
decodeValue :: ∀ a. Decoder a -> Value -> Result String a
decodeValue (Decoder decoder) val = decoder val


-- | Create a custom decoder that may do some fancy computation.
customDecoder :: ∀ a b. Decoder a -> (a -> Result String b) -> Decoder b
customDecoder (Decoder decoder) func =
    Decoder $ decoder >=> func



-- | Sometimes you have JSON with recursive structure, like nested comments.
-- | You can use `lazy` to make sure your decoder unrolls lazily.
-- |
-- |     type alias Comment =
-- |       { message : String
-- |       , responses : Responses
-- |       }
-- |
-- |     type Responses = Responses (List Comment)
-- |
-- |     comment : Decoder Comment
-- |     comment =
-- |       map2 Comment
-- |         (field "message" string)
-- |         (field "responses" (map Responses (list (lazy (\_ -> comment)))))
-- |
-- | If we had said `list comment` instead, we would start expanding the value
-- | infinitely. What is a `comment`? It is a decoder for objects where the
-- | `responses` field contains comments. What is a `comment` though? Etc.
-- |
-- | By using `list (lazy (\_ -> comment))` we make sure the decoder only expands
-- | to be as deep as the JSON we are given. You can read more about recursive data
-- | structures [here][].
-- |
-- | [here]: https://github.com/elm-lang/elm-compiler/blob/master/hints/recursive-alias.md
-- |
-- | This function was added in Elm 0.18.
lazy :: ∀ a. (Unit -> Decoder a) -> Decoder a
lazy thunk =
    pure unit >>= thunk


-- | Ignore the JSON and make the decoder fail. This is handy when used with
-- | `oneOf` or `andThen` where you want to give a custom error message in some
-- | case.
fail :: ∀ a. String -> Decoder a
fail = Decoder <<< const <<< Err


-- | Ignore the JSON and produce a certain Elm value.
-- |
-- |     decodeString (succeed 42) "true"    == Ok 42
-- |     decodeString (succeed 42) "[1,2,3]" == Ok 42
-- |     decodeString (succeed 42) "hello"   == Err ... -- this is not a valid JSON string
-- |
-- | This is handy when used with `oneOf` or `andThen`.
succeed :: ∀ a. a -> Decoder a
succeed = pure


-- TUPLES

-- | Handle an array with exactly one element.
-- |
-- |     extractString :: Decoder String
-- |     extractString =
-- |         tuple1 identity string
-- |
-- |     authorship :: Decoder String
-- |     authorship =
-- |         oneOf
-- |           [ tuple1 (\author -> "Author: " <> author) string
-- |           , list string |> map (\authors -> "Co-authors: " <> String.join ", " authors)
-- |           ]
-- |
-- | This function was removed in Elm 0.18.
tuple1 :: ∀ a value. (a -> value) -> Decoder a -> Decoder value
tuple1 func decoder0 =
    Decoder $ \val -> do
        values <- tryArray val 1
        v0 <- tryIndex values decoder0 0
        pure (func v0)


tryArray :: Value -> Int -> Result String (Array Value)
tryArray val expected = do
    arr <- toResult $ readArray val
    let len = Array.length arr
    if len == expected
        then Ok arr
        else Err $ "Expected array with exact length: " <> (show expected) <> ", but got length: " <> (show len)


tryIndex :: ∀ a. Array Value -> Decoder a -> Int -> Result String a
tryIndex arr (Decoder decoder) i = do
    val <- fromMaybe
        "Internal error getting index"
        (Array.index arr i)

    decoder val


-- | Handle an array with exactly two elements. Useful for points and simple
-- | pairs.
-- |
-- |     -- [3,4] or [0,0]
-- |     point :: Decoder (Tuple Float Float)
-- |     point =
-- |         tuple2 Tuple float float
-- |
-- |     -- ["John","Doe"] or ["Hermann","Hesse"]
-- |     name :: Decoder Name
-- |     name =
-- |         tuple2 Name string string
-- |
-- |     type Name = { first :: String, last :: String }
-- |
-- | This function was removed in Elm 0.18.
tuple2 :: ∀ a b value. (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
tuple2 func d0 d1 =
    Decoder $ \val -> do
        values <- tryArray val 2

        v0 <- tryIndex values d0 0
        v1 <- tryIndex values d1 1

        pure (func v0 v1)


-- | Handle an array with exactly three elements.
-- |
-- | This function was removed in Elm 0.18.
tuple3 :: ∀ a b c value. (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
tuple3 func d0 d1 d2 =
    Decoder $ \val -> do
        values <- tryArray val 3

        v0 <- tryIndex values d0 0
        v1 <- tryIndex values d1 1
        v2 <- tryIndex values d2 2

        pure (func v0 v1 v2)


-- | This function was removed in Elm 0.18.
tuple4 :: ∀ a b c d value. (a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder value
tuple4 func d0 d1 d2 d3 =
    Decoder $ \val -> do
        values <- tryArray val 4

        v0 <- tryIndex values d0 0
        v1 <- tryIndex values d1 1
        v2 <- tryIndex values d2 2
        v3 <- tryIndex values d3 3

        pure (func v0 v1 v2 v3)


-- | This function was removed in Elm 0.18.
tuple5 :: ∀ a b c d e value. (a -> b -> c -> d -> e -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder value
tuple5 func d0 d1 d2 d3 d4 =
    Decoder $ \val -> do
        values <- tryArray val 5

        v0 <- tryIndex values d0 0
        v1 <- tryIndex values d1 1
        v2 <- tryIndex values d2 2
        v3 <- tryIndex values d3 3
        v4 <- tryIndex values d4 4

        pure (func v0 v1 v2 v3 v4)


-- | This function was removed in Elm 0.18.
tuple6 :: ∀ a b c d e f value. (a -> b -> c -> d -> e -> f -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder value
tuple6 func d0 d1 d2 d3 d4 d5 =
    Decoder $ \val -> do
        values <- tryArray val 6

        v0 <- tryIndex values d0 0
        v1 <- tryIndex values d1 1
        v2 <- tryIndex values d2 2
        v3 <- tryIndex values d3 3
        v4 <- tryIndex values d4 4
        v5 <- tryIndex values d5 5

        pure (func v0 v1 v2 v3 v4 v5)


-- | This function was removed in Elm 0.18.
tuple7 :: ∀ a b c d e f g value. (a -> b -> c -> d -> e -> f -> g -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder value
tuple7 func d0 d1 d2 d3 d4 d5 d6 =
    Decoder $ \val -> do
        values <- tryArray val 7

        v0 <- tryIndex values d0 0
        v1 <- tryIndex values d1 1
        v2 <- tryIndex values d2 2
        v3 <- tryIndex values d3 3
        v4 <- tryIndex values d4 4
        v5 <- tryIndex values d5 5
        v6 <- tryIndex values d6 6

        pure (func v0 v1 v2 v3 v4 v5 v6)


-- | This function was removed in Elm 0.18.
tuple8 :: ∀ a b c d e f g h value. (a -> b -> c -> d -> e -> f -> g -> h -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder value
tuple8 func d0 d1 d2 d3 d4 d5 d6 d7 =
    Decoder $ \val -> do
        values <- tryArray val 8

        v0 <- tryIndex values d0 0
        v1 <- tryIndex values d1 1
        v2 <- tryIndex values d2 2
        v3 <- tryIndex values d3 3
        v4 <- tryIndex values d4 4
        v5 <- tryIndex values d5 5
        v6 <- tryIndex values d6 6
        v7 <- tryIndex values d7 7

        pure (func v0 v1 v2 v3 v4 v5 v6 v7)

