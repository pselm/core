
-- | A way to turn Json values into Elm values. A `Decoder a` represents a
-- | decoding operation that will either produce a value of type `a`, or fail.
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
    , extractForeign
    , string, int, float, bool, null
    , list, array, unfoldable
    , tuple1, tuple2, tuple3, tuple4, tuple5, tuple6, tuple7, tuple8
    , field, (:=), at
    , object1, object2, object3, object4, object5, object6, object7, object8
    , keyValuePairs, dict
    , oneOf, maybe, fail, succeed
    , value, customDecoder
    ) where


import Prelude (map) as Virtual
import Elm.Json.Encode (Value) as Virtual
import Elm.Bind (andThen)
import Elm.Apply (andMap, map2, map3, map4, map5)

import Data.Foreign (Foreign, ForeignError(..), F, readArray, isNull, isUndefined, typeOf, parseJSON)
import Data.Foreign.Class (class IsForeign, read)
import Data.Foreign.Index (prop)
import Data.Traversable (traverse)
import Data.Monoid (class Monoid)
import Data.Foldable (class Foldable, foldl, foldMap)
import Data.Unfoldable (class Unfoldable, unfoldr)
import Elm.Json.Encode (Value)
import Control.Apply (lift2, lift3, lift4, lift5)
import Control.Alt (class Alt, alt)
import Control.Bind ((>=>))
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
    , show, ($), (<<<), (<$>), (<*>), const, (==), (<>)
    )


-- | Represents a way of decoding JSON values. If you have a `(Decoder (List String))`
-- | it will attempt to take some JSON value and turn it into a list of strings.
-- | These decoders are easy to put together so you can create more and more complex
-- | decoders.
newtype Decoder a = Decoder (Value -> Result String a)


{- The Foreign module uses `Either ForeignError a` to pass errors around,
whereas we need `Result String a` Actually, at first I thought we could work
with `Either ForeignError a` internally (converting at the last minute), but
the problem is the signature for `customDecoder`.
-}
toResult :: ∀ a. F a -> Result String a
toResult (Right a) = Ok a
toResult (Left err) = Err (show err)


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


-- | Using a certain decoder, attempt to parse a JSON string. If the decoder
-- | fails, you will get a string message telling you why.
-- |
-- |     decodeString (tuple2 (,) Tuple float float) "[3,4]"                  -- Ok (Tuple 3 4)
-- |     decodeString (tuple2 (,) Tuple float float) "{ \"x\": 3, \"y\": 4 }" -- Err ""
decodeString :: ∀ a. Decoder a -> String -> Result String a
decodeString (Decoder decoder) str =
    toResult (parseJSON str) >>= decoder


-- OBJECTS

-- | Access a nested field, making it easy to dive into big structures. This is
-- | really a helper function so you do not need to write `(:=)` so many times.
-- |
-- |     -- object.target.value = 'hello'
-- |     value :: Decoder String
-- |     value =
-- |         at ["target", "value"] string
-- |
-- | It is defined as
-- |
-- |     at fields decoder =
-- |         List.foldr (:=) decoder fields
-- |
-- | Note that the signature is defined in terms of `Foldable` so that it will
-- | work with `Array` or `List` (among others).
at :: ∀ f a. (Foldable f) => f String -> Decoder a -> Decoder a
at fields decoder =
    foldr (:=) decoder fields


-- | Applies the decoder to the field with the given name.
-- | Fails if the JSON object has no such field.
-- |
-- |     nameAndAge :: Decoder (Tuple String Int)
-- |     nameAndAge =
-- |         object2 Tuple
-- |           ("name" := string)
-- |           ("age" := int)
-- |
-- |     optionalProfession :: Decoder (Maybe String)
-- |     optionalProfession =
-- |         maybe ("profession" := string)
field :: ∀ a. String -> Decoder a -> Decoder a
field f (Decoder decoder) =
    Decoder $ \val ->
        toResult (prop f val) >>= decoder

infixl 4 field as :=


-- | Apply a function to a decoder.
-- |
-- |     object1 sqrt ("x" := float)
-- |
-- | Equivalent to Purescript's `map`.
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
object3 :: ∀ a b c value. (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
object3 = lift3


-- | Equivalent to Purescript's `lift4`.
object4 :: ∀ a b c d value. (a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder value
object4 = lift4


-- | Equivalent to Purescript's `lift5`.
object5 :: ∀ a b c d e value. (a -> b -> c -> d -> e -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder value
object5 = lift5


object6 :: ∀ a b c d e f value. (a -> b -> c -> d -> e -> f -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder value
object6 func a b c d e f =
    func <$> a <*> b <*> c <*> d <*> e <*> f


object7 :: ∀ a b c d e f g value. (a -> b -> c -> d -> e -> f -> g -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder value
object7 func a b c d e f g =
    func <$> a <*> b <*> c <*> d <*> e <*> f <*> g


object8 :: ∀ a b c d e f g h value. (a -> b -> c -> d -> e -> f -> g -> h -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder value
object8 func a b c d e f g h =
    func <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h


-- | Turn any object into a list of key-value pairs, including inherited enumerable properties. Fails if _any_ value can't be
-- | decoded with the given decoder.
-- |
-- |     -- { "tom": 89, "sue": 92, "bill": 97, ... }
-- |     grades :: Decoder (List (Tuple String Int))
-- |     grades =
-- |         keyValuePairs int
-- |
-- | The container for the return type is polymorphic in order to accommodate `List` or `Array`, among others.
keyValuePairs :: ∀ f a. (Monoid (f (Tuple String a)), Applicative f) => Decoder a -> Decoder (f (Tuple String a))
keyValuePairs (Decoder decoder) =
    Decoder $ \val -> do
        ks <- keys val
        arr <- traverse (\key -> do
            p <- toResult $ prop key val
            d <- decoder p
            pure $ Tuple key d
        ) ks
        pure $ foldMap pure arr


-- | Get an array of the keys defined on a foreign value.
-- |
-- | This is from Data.Foreign.Keys, except that it used hasOwnProperty, whereas
-- | the Elm version also includes inherited properties.
keys :: Foreign -> Result String (Array String)
keys val | isNull val = toResult $ Left $ TypeMismatch "object" "null"
keys val | isUndefined val = toResult $ Left $ TypeMismatch "object" "undefined"
keys val | typeOf val == "object" = Ok $ unsafeKeys val
keys val = toResult $ Left $ TypeMismatch "object" (typeOf val)

-- | From Data.Foreign, except includes inherited properties (as the Elm equivalent does)
foreign import unsafeKeys :: Foreign -> Array String


-- | Turn any object into a dictionary of key-value pairs, including inherited enumerable properties. Fails if _any_ value can't be
-- | decoded with the given decoder.
-- |
-- |     -- { "mercury": 0.33, "venus": 4.87, "earth": 5.97, ... }
-- |     planetMasses :: Decoder (Dict String Float)
-- |     planetMasses =
-- |         dict float
dict :: ∀ a. Decoder a -> Decoder (Dict String a)
dict decoder =
    map Dict.fromList (keyValuePairs decoder)


-- | Try out multiple different decoders. This is helpful when you are dealing
-- | with something with a very strange shape and when `andThen` does not help
-- | narrow things down so you can be more targeted.
-- |
-- |     -- [ [3,4], { "x":0, "y":0 }, [5,12] ]
-- |
-- |     points :: Decoder (List (Tuple Float Float))
-- |     points =
-- |         list point
-- |
-- |     point :: Decoder (Tuple Float Float)
-- |     point =
-- |         oneOf
-- |         [ tuple2 Tuple float float
-- |         , object2 Tuple ("x" := float) ("y" := float)
-- |         ]
-- |
-- | The container has a polymorphic type to accommodate `List` or `Array`,
-- | among others.
oneOf :: ∀ f a. (Foldable f) => f (Decoder a) -> Decoder a
oneOf decoders =
    foldl alt (fail "Expected one of: ") decoders


-- | Extract any value with an `IsForeign` instance.
-- |
-- | Note that this is not in the Elm API.
extractForeign :: ∀ a. (IsForeign a) => Decoder a
extractForeign = Decoder $ toResult <<< read


-- | Extract a string.
-- |
-- |     -- ["John","Doe"]
-- |
-- |     name :: Decoder (Tuple String String)
-- |     name =
-- |         tuple2 Tuple string string
string :: Decoder String
string = extractForeign


-- | Extract a float.
-- |
-- |     -- [ 6.022, 3.1415, 1.618 ]
-- |
-- |     numbers :: Decoder (List Float)
-- |     numbers =
-- |         list float
float :: Decoder Float
float = extractForeign


-- | Extract an integer.
-- |
-- |     -- { ... "age": 42 ... }
-- |
-- |     age :: Decoder Int
-- |     age =
-- |         "age" := int
int :: Decoder Int
int = extractForeign


-- | Extract a boolean.
-- |
-- |     -- { ... "checked": true ... }
-- |
-- |     checked :: Decoder Bool
-- |     checked =
-- |         "checked" := bool
bool :: Decoder Bool
bool = extractForeign


-- | Extract a List from a JS array.
-- |
-- |     -- [1,2,3,4]
-- |
-- |     numbers :: Decoder (List Int)
-- |     numbers =
-- |         list int
list :: ∀ a. Decoder a -> Decoder (List a)
list (Decoder decoder) =
    Decoder $ \val -> do
        arr <- toResult $ readArray val
        List.fromFoldable <$> traverse decoder arr


-- | Extract an Array from a JS array.
-- |
-- |     -- [1,2,3,4]
-- |
-- |     numbers :: Decoder (Array Int)
-- |     numbers =
-- |         array int
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


-- | Decode null as the value given, and fail otherwise. Primarily useful for
-- | creating *other* decoders.
-- |
-- |     numbers :: Decoder (Array Int)
-- |     numbers =
-- |         list (oneOf [ int, null 0 ])
-- |
-- | This decoder treats `null` as `Nothing`, and otherwise tries to produce a
-- | `Just`.
-- |
-- |     nullOr :: Decoder a -> Decoder (Maybe a)
-- |     nullOr decoder =
-- |         oneOf
-- |         [ null Nothing
-- |         , map Just decoder
-- |         ]
null :: ∀ a. a -> Decoder a
null default =
    Decoder $
        \val ->
            if isNull val
                then Ok default
                else toResult $ Left $ TypeMismatch "null" (typeOf val)



-- | Extract a Maybe value, wrapping successes with `Just` and turning any
-- | failure in `Nothing`. If you are expecting that a field can sometimes be `null`,
-- | it's better to check for it explicitly, as this function will swallow
-- | errors from ill-formed JSON.
-- |
-- | The following code decodes JSON objects that may not have a profession field.
-- |
-- |     -- profession: Just "plumber"
-- |     -- { name: "Tom", age: 31, profession: "plumber" }
-- |     -- profession: Nothing
-- |     -- { name: "Sue", age: 42 }
-- |     -- { name: "Amy", age: 27, profession: null }
-- |     -- { name: "Joe", age: 36, profession: ["something", "unexpected"] }
-- |
-- |     type Person =
-- |         { name :: String
-- |         , age :: Int
-- |         , profession :: Maybe String
-- |         }
-- |
-- |     person :: Decoder Person
-- |     person =
-- |         object3 Person
-- |           ("name" := string)
-- |           ("age" := int)
-- |           (maybe ("profession" := string))
maybe :: ∀ a. Decoder a -> Decoder (Maybe a)
maybe (Decoder decoder) =
    Decoder $ \val ->
        case decoder val of
            Ok decoded ->
                Ok (Just decoded)

            Err _ ->
                Ok Nothing


-- | Bring in an arbitrary JSON value. Useful if you need to work with crazily
-- | formatted data. For example, this lets you create a parser for "variadic" lists
-- | where the first few types are different, followed by 0 or more of the same
-- | type.
-- |
-- |     variadic2 :: (a -> b -> List c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
-- |     variadic2 f a b c =
-- |         let
-- |             combineResults = List.foldr (Result.map2 (::)) (Ok [])
-- |         in
-- |             customDecoder (list value) (\jsonList ->
-- |                 case jsonList of
-- |                   one :: two :: rest ->
-- |                       Result.map3 f
-- |                         (decodeValue a one)
-- |                         (decodeValue b two)
-- |                         (combineResults (List.map (decodeValue c) rest))
-- |
-- |                   _ -> Result.Err "expecting at least two elements in the array")
value :: Decoder Value
value = Decoder $ Ok


-- | Using a certain decoder, attempt to parse a raw `Json.Value`.
decodeValue :: ∀ a. Decoder a -> Value -> Result String a
decodeValue (Decoder decoder) val = decoder val


-- | Create a custom decoder that may do some fancy computation.
customDecoder :: ∀ a b. Decoder a -> (a -> Result String b) -> Decoder b
customDecoder (Decoder decoder) func =
    Decoder $ decoder >=> func


-- | A decoder that always fails. Useful when paired with `andThen` or `oneOf`
-- | to improve error messages when things go wrong. For example, the following
-- | decoder is able to provide a much more specific error message when `fail` is
-- | the last option.
-- |
-- |     point :: Decoder (Tuple Float Float)
-- |     point =
-- |         oneOf
-- |         [ tuple2 Tuple float float
-- |         , object2 Tuple ("x" := float) ("y" := float)
-- |         , fail "expecting some kind of point"
-- |         ]
fail :: ∀ a. String -> Decoder a
fail = Decoder <<< const <<< Err


-- | A decoder that always succeeds. Useful when paired with `andThen` or
-- | `oneOf` but everything is supposed to work out at the end. For example,
-- | maybe you have an optional field that can have a default value when it is
-- | missing.
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
tryIndex arr (Decoder decoder) index = do
    val <- fromMaybe
        "Internal error getting index"
        (Array.index arr index)

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
tuple2 :: ∀ a b value. (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
tuple2 func d0 d1 =
    Decoder $ \val -> do
        values <- tryArray val 2

        v0 <- tryIndex values d0 0
        v1 <- tryIndex values d1 1

        pure (func v0 v1)


-- | Handle an array with exactly three elements.
tuple3 :: ∀ a b c value. (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
tuple3 func d0 d1 d2 =
    Decoder $ \val -> do
        values <- tryArray val 3

        v0 <- tryIndex values d0 0
        v1 <- tryIndex values d1 1
        v2 <- tryIndex values d2 2

        pure (func v0 v1 v2)


tuple4 :: ∀ a b c d value. (a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder value
tuple4 func d0 d1 d2 d3 =
    Decoder $ \val -> do
        values <- tryArray val 4

        v0 <- tryIndex values d0 0
        v1 <- tryIndex values d1 1
        v2 <- tryIndex values d2 2
        v3 <- tryIndex values d3 3

        pure (func v0 v1 v2 v3)


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

