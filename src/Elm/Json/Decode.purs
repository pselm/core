
-- | Turn JSON values into Elm values. Definitely check out this [intro to
-- | JSON decoders][guide] to get a feel for how this library works!
-- |
-- | [guide]: https://guide.elm-lang.org/interop/json.html
-- |
-- | Elm's `Json.Decode` doesn't seem to be quite like any existing Purescript
-- | package, so I've re-implemented it, using parts of purescript-foreign as
-- | a base. For other approaches to decoding JSON in Purescript, you could see
-- | purescript-foreign, and the purescript-argonaut-* packages.
-- |
-- | Note that (so far) we're not trying to preserve all the nice error messages
-- | that Elm gives ... we could do a better job of that.

module Elm.Json.Decode
    ( module Virtual
    , Decoder
    , decodeString, decodeValue
    , fromForeign
    , string, int, float, bool, null
    , list, array, unfoldable
    , tuple1, tuple2, tuple3, tuple4, tuple5, tuple6, tuple7, tuple8
    , field, (:=), at, index
    , object1, object2, object3, object4, object5, object6, object7, object8
    , keyValuePairs, dict
    , oneOf, nullable, maybe, fail, succeed, succeed_
    , value, customDecoder, lazy
    , equalDecoders
    ) where


import Control.Alt (class Alt, alt, (<|>))
import Control.Apply (lift2, lift3, lift4, lift5)
import Control.Bind ((>=>))
import Control.Monad.Except (runExcept)
import Data.Array (uncons)
import Data.Array as Array
import Data.Coyoneda (Coyoneda, coyoneda, unCoyoneda)
import Data.Either (Either(..))
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (class Foldable, foldl, foldMap)
import Data.Foreign (Foreign, ForeignError(..), F, readArray, readString, readBoolean, readNumber, readInt, isNull, isUndefined, typeOf)
import Data.Foreign as DF
import Data.Foreign.Index (readIndex, readProp)
import Data.Foreign.JSON (parseJSON)
import Data.Leibniz (type (~), coerceSymm)
import Data.Monoid (class Monoid)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Elm.Apply (andMap, map2, map3, map4, map5, map6, map7, map8) as Virtual
import Elm.Apply (map6, map7, map8)
import Elm.Basics (Bool, Float)
import Elm.Bind (andThen) as Virtual
import Elm.Dict (Dict)
import Elm.Dict as Dict
import Elm.Json.Encode (Value)
import Elm.Json.Encode (Value) as Virtual
import Elm.List (List, foldr)
import Elm.Maybe (Maybe(..))
import Elm.Result (Result(Err, Ok))
import Prelude (class Applicative, class Apply, class Bind, class Eq, class Functor, class Monad, Unit, apply, bind, const, discard, eq, id, map, pure, show, unit, void, (#), ($), (&&), (<$>), (<<<), (<>), (==), (>>=), (>>>), (||))
import Prelude (map) as Virtual
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (reallyUnsafeRefEq, unsafeRefEq)


-- | A value that knows how to decode JSON values.
--
-- We collect some evidence about what the `a` is, in cases where it's not
-- fully polymorphic.
--
-- The `Run` constructor is probably an example of something more general that
-- I'm not seeing yet.
--
-- This little DSL could probably be sipmlified a bit ...  ideally, it should
-- consist of irreducible things.
data Decoder a
    = Ap (ApplyCoyoneda Decoder a)
    | Array (a ~ Array Foreign)
    | ArrayOfLength (a ~ Array Foreign) Int
    | Bind (BindCoyoneda Decoder a)
    | Fail String
    | Field (a ~ Foreign) String
    | FromForeign (Foreign -> F a)
    | Index (a ~ Foreign) Int
    | Keys (a ~ Array String)
    | Map (Coyoneda Decoder a)
    | Null (Maybe (a -> a -> Bool)) a
    | OneOf (Decoder a) (Decoder a)
    | Run (Decoder Foreign) (Decoder a)
    | Succeed (Maybe (a -> a -> Bool)) a
    | Value (a ~ Foreign)


-- The Foreign module uses `Except` to pass errors around, whereas we need
-- `Result String a`.
--
-- We should render the err case a little more nicely.
toResult :: ∀ a. F a -> Result String a
toResult f =
    case runExcept f of
        Right a -> Ok a
        Left err -> Err (show err)


-- | Run a `Decoder` on some JSON `Value`. You can send these JSON values
-- | through ports, so that is probably the main time you would use this function.
decodeValue :: ∀ a. Decoder a -> Value -> Result String a
decodeValue =
    -- This gets called recursively without tail-calls, so it will use stack
    -- space. However, a decoder should not be constructed in a way that chews
    -- through too much stack space. If it turns out to be a problem, we can
    -- make this stack-safe in one way or another.
    case _ of
        Ap coyo ->
            \val ->
                coyo # unApplyCoyoneda
                    (\tagger decoder ->
                        apply ((decodeValue tagger) val) ((decodeValue decoder) val)
                    )

        Array proof ->
            readArray >>> toResult >>> map (coerceSymm proof)

        ArrayOfLength proof expected ->
            \val -> map (coerceSymm proof) $ do
                arr <- toResult $ readArray val
                let len = Array.length arr
                if len == expected
                    then Ok arr
                    else Err $ "Expected array with exact length: " <> (show expected) <> ", but got length: " <> (show len)

        Bind coyo ->
            \val ->
                coyo # unBindCoyoneda
                    (\decoder func ->
                        case (decodeValue decoder) val of
                            Err err ->
                                Err err

                            Ok a ->
                                decodeValue (func a) val
                    )

        Fail err ->
            const $ Err err

        Field proof key ->
            readProp key >>> toResult >>> map (coerceSymm proof)

        FromForeign func ->
            func >>> toResult

        Index proof i ->
            readIndex i >>> toResult >>> map (coerceSymm proof)

        Keys proof ->
            keys >>> map (coerceSymm proof)

        Map coyo ->
            coyo # unCoyoneda
                (\tagger decoder ->
                    map tagger <<< (decodeValue decoder)
                )

        Null _ a ->
            \val ->
                if isNull val
                    then Ok a
                    else toResult $ DF.fail $ TypeMismatch "null" (typeOf val)

        OneOf left right ->
            -- There is probably a more elevant implementation possible.
            \val ->
                case (decodeValue left) val of
                    Ok result ->
                        Ok result

                    Err leftErr ->
                        case (decodeValue right) val of
                            Ok result ->
                                Ok result

                            Err rightErr ->
                                Err $ leftErr <> " <|> " <> rightErr

        Run decodeForeign decoder ->
            (decodeValue decodeForeign) >=> (decodeValue decoder)

        Succeed _ a ->
            const $ Ok a

        Value proof ->
            id >>> coerceSymm proof >>> Ok


-- | For cases where we have a witness that the two decoders are the same type.
unsafeCoerceDecoder :: ∀ a b. Decoder a -> Decoder b
unsafeCoerceDecoder = unsafeCoerce


-- | Tries to compare two decoders for equality. Subject to false negatives,
-- | but positives should be reliatble.
-- |
-- | If the `a` type has an `Eq` instance, then use `ersatzEqDecoders` instead
-- | to help with the `succeed` case.
equalDecoders :: ∀ a. Decoder a -> Decoder a -> Bool
equalDecoders d1 d2 =
    unsafeRefEq d1 d2 || case d1, d2 of
        Ap coyoLeft, Ap coyoRight ->
            coyoLeft # unApplyCoyoneda (\tagger1 decoder1 ->
            coyoRight # unApplyCoyoneda (\tagger2 decoder2 ->
                -- In this case, we've got decoders on both sides, and we don't
                -- know what type we're coming from. So, the best we can do (at
                -- least for now) is check referntial equality of each side.
                -- If either side has referntial equality, then we know the
                -- types match.
                case reallyUnsafeRefEq tagger1 tagger2, reallyUnsafeRefEq decoder1 decoder2 of
                    true, true ->
                         -- If they are both referentially equal, then we're
                         -- equal
                         true

                    false, false ->
                        -- If neither are referntially equal, we don't have a
                        -- witness to the fact that we're coming from the same
                        -- type. So, at least for now, there's nothing we can do.
                        false

                    true, false ->
                        -- If the taggers are equal, at least we have the same types,
                        -- so we can try on the decoders.
                        equalDecoders decoder1 (unsafeCoerceDecoder decoder2)

                    false, true ->
                        -- If the decoders are equal, we have the same type, so try
                        -- on the taggers.
                        equalDecoders tagger1 (unsafeCoerceDecoder tagger2)
            ))

        Array _, Array _ ->
            true

        ArrayOfLength _ leftN, ArrayOfLength _ rightN ->
            leftN == rightN

        Bind coyoLeft, Bind coyoRight ->
            coyoLeft # unBindCoyoneda (\decoder1 func1 ->
            coyoRight # unBindCoyoneda (\decoder2 func2 ->
                if reallyUnsafeRefEq func1 func2 then
                    -- The fact that func1 and func2 are referentially
                    -- equal is our warrant for believing that decoder1 and
                    -- decoder2 have the same type.
                    equalDecoders decoder1 (unsafeCoerceDecoder decoder2)
                else
                    false
            ))

        Fail reasonLeft, Fail reasonRight ->
            reasonLeft == reasonRight

        Field _ keyLeft, Field _ keyRight ->
            keyLeft == keyRight

        FromForeign funcLeft, FromForeign funcRight ->
            unsafeRefEq funcLeft funcRight

        Index _ indexLeft, Index _ indexRight ->
            indexLeft == indexRight

        Keys _, Keys _ ->
            true

        Map coyoLeft, Map coyoRight ->
            coyoLeft # unCoyoneda (\tagger1 decoder1 ->
            coyoRight # unCoyoneda (\tagger2 decoder2 ->
                if reallyUnsafeRefEq tagger1 tagger2 then
                    -- The fact that tagger1 and tagger2 are referentially
                    -- equal is our warrant for believing that decoder1 and
                    -- decoder2 have the same type.
                    equalDecoders decoder1 (unsafeCoerceDecoder decoder2)
                else
                    false
            ))

        Null (Just equals) a, Null _ b ->
            unsafeRefEq a b || equals a b

        Null _ a, Null (Just equals) b ->
            unsafeRefEq a b || equals a b

        Null _ a, Null _ b ->
            unsafeRefEq a b

        OneOf leftLeft leftRight, OneOf rightLeft rightRight ->
            equalDecoders leftLeft rightLeft &&
            equalDecoders leftRight rightRight

        Run leftLeft leftRight, Run rightLeft rightRight ->
            equalDecoders leftLeft rightLeft &&
            equalDecoders leftRight rightRight

        Succeed (Just equals) a, Succeed _ b ->
            unsafeRefEq a b || equals a b

        Succeed _ a, Succeed (Just equals) b ->
            unsafeRefEq a b || equals a b

        Succeed _ a, Succeed _ b ->
            unsafeRefEq a b

        Value _, Value _ ->
            true

        _, _ ->
            false


-- For all the instances we need, we're basically just collecting the inputs,
-- so we can run them later. We apply the Coyoneda strategry to the `Apply` and
-- `Bind` instances as well. (There may well be a better way to do that, e.g. a
-- free monad?)
instance functorDecoder :: Functor Decoder where
    map func decoder =
        Map $ coyoneda func decoder


instance altDecoder :: Alt Decoder where
    alt = OneOf


-- This is the coyoneda strategy, but for Apply rather than Functor ... not
-- sure it this is a thing or not.
data ApplyCoyonedaF f a i = ApplyCoyonedaF (f (i -> a)) (f i)

newtype ApplyCoyoneda f a = ApplyCoyoneda (Exists (ApplyCoyonedaF f a))

applyCoyoneda :: ∀ f a b. f (a -> b) -> f a -> ApplyCoyoneda f b
applyCoyoneda k fi = ApplyCoyoneda $ mkExists $ ApplyCoyonedaF k fi

unApplyCoyoneda :: ∀ f a r. (∀ b. f (b -> a) -> f b -> r) -> ApplyCoyoneda f a -> r
unApplyCoyoneda f (ApplyCoyoneda e) = runExists (\(ApplyCoyonedaF k fi) -> f k fi) e


-- It's possible that this is unnecessary if I just used a free monad ...  that
-- is, I might be able to get this for free.
instance applyDecoder :: Apply Decoder where
    apply f g =
        Ap $ applyCoyoneda f g


instance applicativeDecoder :: Applicative Decoder where
    pure = succeed_


-- This is the coyoneda strategy, but for Bind rather than Functor ... not
-- sure it this is a thing or not.
data BindCoyonedaF f a i = BindCoyonedaF (f i) (i -> f a)

newtype BindCoyoneda f a = BindCoyoneda (Exists (BindCoyonedaF f a))

bindCoyoneda :: ∀ f a b. f a -> (a -> f b) -> BindCoyoneda f b
bindCoyoneda fi k = BindCoyoneda $ mkExists $ BindCoyonedaF fi k

unBindCoyoneda :: ∀ f a r. (∀ b. f b -> (b -> f a) -> r) -> BindCoyoneda f a -> r
unBindCoyoneda f (BindCoyoneda e) = runExists (\(BindCoyonedaF k fi) -> f k fi) e


-- Again, one wonders whether I'm really looking for a free monad here.
instance bindDecoder :: Bind Decoder where
    bind decoder func =
        Bind $ bindCoyoneda decoder func


instance monadDecoder :: Monad Decoder


-- These are some partially-applied constructors from which other things
-- can be built.
arrayT :: Decoder (Array Foreign)
arrayT = Array id

arrayOfLengthT :: Int -> Decoder (Array Foreign)
arrayOfLengthT = ArrayOfLength id

fieldT :: String -> Decoder Foreign
fieldT = Field id

indexT :: Int -> Decoder Foreign
indexT = Index id

keysT :: Decoder (Array String)
keysT = Keys id

valueT :: Decoder Value
valueT = Value id


-- | Parse the given string into a JSON value and then run the `Decoder` on it.
-- | This will fail if the string is not well-formed JSON or if the `Decoder`
-- | fails for some reason.
-- |
-- |     decodeString int "4"     == Ok 4
-- |     decodeString int "1 + 2" == Err ...
decodeString :: ∀ a. Decoder a -> String -> Result String a
decodeString decoder str =
    toResult (parseJSON str) >>= (decodeValue decoder)


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
index = Run <<< indexT


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
field = Run <<< fieldT


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
keyValuePairs decoder = do
    arr <-
        keysT >>= traverse
            (\key -> do
                prop <- field key decoder
                pure $ Tuple key prop
            )
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
oneOf =
    foldl alt (Fail "Expected one of: ")


-- | Given a function which reads a `Foreign`, make a decoder.
-- |
-- | Note that this is not in the Elm API.
fromForeign :: ∀ a. (Foreign -> F a) -> Decoder a
fromForeign = FromForeign


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
list = unfoldable


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
unfoldable decoder = do
    arr <- arrayT >>= traverse (\val -> Run (succeed_ val) decoder)
    pure $ unfoldr
        (\a -> map (\b -> Tuple b.head b.tail) (uncons a))
        arr


-- | Decode a `null` value into some Elm value.
-- |
-- |     decodeString (null False) "null" == Ok False
-- |     decodeString (null 42) "null"    == Ok 42
-- |     decodeString (null 42) "42"      == Err ..
-- |     decodeString (null 42) "false"   == Err ..
-- |
-- | So if you ever see a `null`, this will return whatever value you specified.
null :: ∀ a. Eq a => a -> Decoder a
null = Null (Just eq)


-- | LIke `null`, but for cases where your default value does not have an `Eq`
-- | instance. Use `null` where you can, because it will make `equalDecoders`
-- | more reliable.
null_ :: ∀ a. a -> Decoder a
null_ = Null Nothing


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
    null_ Nothing <|> map Just decoder


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
-- |
-- | `equalDecoders` will consider the results of this function to be equal if
-- | the provided decoder is equal ... that is, `maybe` is equal-preserving.
maybe :: ∀ a. Decoder a -> Decoder (Maybe a)
maybe decoder =
    map Just decoder <|> succeed_ Nothing


-- | Do not do anything with a JSON value, just bring it into Elm as a `Value`.
-- | This can be useful if you have particularly crazy data that you would like to
-- | deal with later. Or if you are going to send it out a port and do not care
-- | about its structure.
-- |
-- | Works with `equalDecoders`
value :: Decoder Value
value = valueT


-- | Create a custom decoder that may do some fancy computation.
customDecoder :: ∀ a b. Decoder a -> (a -> Result String b) -> Decoder b
customDecoder decoder func =
    -- This should work, but it may end up not playing well with `equalDecoder`
    -- ... I might have to capture the decoder and func in the DSL. Because the
    -- function I'm captured in the `Bind` is a lmbda, constructed fresh each
    -- time, so it won't be referentially equal, even if it's the same func.
    decoder >>=
        \a ->
            case func a of
                Ok ok ->
                    succeed_ ok

                Err err ->
                    fail err


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
-- |
-- | This function works with `equalDecoders` so long as you provide functions
-- | that are referentially equal ... see the docs for `equalDecoders` for more
-- | information.  That's probably the best we can do, since the point of
-- | `lazy` is to avoid unrolling the actual decoder until needed.
lazy :: ∀ a. (Unit -> Decoder a) -> Decoder a
lazy = bind (pure unit)


-- | Ignore the JSON and make the decoder fail. This is handy when used with
-- | `oneOf` or `andThen` where you want to give a custom error message in some
-- | case.
-- |
-- | `equalDecoders` considers two `fail` decoders to be equal if they have the
-- | same message.
fail :: ∀ a. String -> Decoder a
fail = Fail


-- | Ignore the JSON and produce a certain Elm value.
-- |
-- |     decodeString (succeed 42) "true"    == Ok 42
-- |     decodeString (succeed 42) "[1,2,3]" == Ok 42
-- |     decodeString (succeed 42) "hello"   == Err ... -- this is not a valid JSON string
-- |
-- | This is handy when used with `oneOf` or `andThen`.
-- |
-- | Works well with `equalDecoders`.
succeed :: ∀ a. Eq a => a -> Decoder a
succeed = Succeed (Just eq)


-- | Like `succeed`, but for cases where your value does not have an
-- | `Eq` instance. Using `succeed` instead will make `equalDecoders`
-- | more reliable -- without an `Eq` instance, we have to rely on
-- | referential equality.
succeed_ :: ∀ a. a -> Decoder a
succeed_ = Succeed Nothing


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
tuple1 func d0 = do
    void $ arrayOfLengthT 1
    func <$> index 0 d0


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
tuple2 func d0 d1 = do
    void $ arrayOfLengthT 2

    v0 <- index 0 d0
    v1 <- index 1 d1

    pure $ func v0 v1


-- | Handle an array with exactly three elements.
-- |
-- | This function was removed in Elm 0.18.
tuple3 :: ∀ a b c value. (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
tuple3 func d0 d1 d2 = do
    void $ arrayOfLengthT 3

    v0 <- index 0 d0
    v1 <- index 1 d1
    v2 <- index 2 d2

    pure $ func v0 v1 v2


-- | This function was removed in Elm 0.18.
tuple4 :: ∀ a b c d value. (a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder value
tuple4 func d0 d1 d2 d3 = do
    void $ arrayOfLengthT 4

    v0 <- index 0 d0
    v1 <- index 1 d1
    v2 <- index 2 d2
    v3 <- index 3 d3

    pure $ func v0 v1 v2 v3


-- | This function was removed in Elm 0.18.
tuple5 :: ∀ a b c d e value. (a -> b -> c -> d -> e -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder value
tuple5 func d0 d1 d2 d3 d4 = do
    void $ arrayOfLengthT 5

    v0 <- index 0 d0
    v1 <- index 1 d1
    v2 <- index 2 d2
    v3 <- index 3 d3
    v4 <- index 4 d4

    pure $ func v0 v1 v2 v3 v4


-- | This function was removed in Elm 0.18.
tuple6 :: ∀ a b c d e f value. (a -> b -> c -> d -> e -> f -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder value
tuple6 func d0 d1 d2 d3 d4 d5 = do
    void $ arrayOfLengthT 6

    v0 <- index 0 d0
    v1 <- index 1 d1
    v2 <- index 2 d2
    v3 <- index 3 d3
    v4 <- index 4 d4
    v5 <- index 5 d5

    pure $ func v0 v1 v2 v3 v4 v5


-- | This function was removed in Elm 0.18.
tuple7 :: ∀ a b c d e f g value. (a -> b -> c -> d -> e -> f -> g -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder value
tuple7 func d0 d1 d2 d3 d4 d5 d6 = do
    void $ arrayOfLengthT 7

    v0 <- index 0 d0
    v1 <- index 1 d1
    v2 <- index 2 d2
    v3 <- index 3 d3
    v4 <- index 4 d4
    v5 <- index 5 d5
    v6 <- index 6 d6

    pure $ func v0 v1 v2 v3 v4 v5 v6


-- | This function was removed in Elm 0.18.
tuple8 :: ∀ a b c d e f g h value. (a -> b -> c -> d -> e -> f -> g -> h -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder value
tuple8 func d0 d1 d2 d3 d4 d5 d6 d7 = do
    void $ arrayOfLengthT 8

    v0 <- index 0 d0
    v1 <- index 1 d1
    v2 <- index 2 d2
    v3 <- index 3 d3
    v4 <- index 4 d4
    v5 <- index 5 d5
    v6 <- index 6 d6
    v7 <- index 7 d7

    pure $ func v0 v1 v2 v3 v4 v5 v6 v7
