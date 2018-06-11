
-- | This module implements parts of how Elm handles "ports" ... that is, Elm's
-- | method for integrating with Javascript.
-- |
-- | When you declare a `port` in Elm, Elm will automatically encode and decode
-- | Elm values to and from certain Javascript representations. This same
-- | mechanism applies when starting your app with `flags`.
-- |
-- | Here are the types Elm will handle via ports and flags.
-- |
-- | | Elm Type | Javascript representation |
-- | | -------- | ------------------------- |
-- | | Boolean  | Boolean                   |
-- | | String   | String                    |
-- | | Int      | Number                    |
-- | | Float    | Number                    |
-- | | List     | Array                     |
-- | | Array    | Array                     |
-- | | Tuple    | Fixed-length, mixed-type array |
-- | | Record   | Object                    |
-- | | Maybe    | Null, or the bare value   |
-- | | Json.Encode.Value | Arbitrary Javascript value |
-- |
-- | These all are given `PortEncoder` and `PortDecoder` instances here, which
-- | encode and decode them in the same way that Elm's "port" mechanism does.
-- | If you should want to send other values through ports, you can create your
-- | own instances if you like. (Or manually convert to and from a `Value`).
-- |
-- | This is something like the `Encode` and `Decode` type-classes that
-- | purescript-foreign-generic provide, but the Javascript representations are
-- | not necessarily equivalent. So, we create our own classes here which
-- | provide the Elm behaviours.
-- |
-- | Since you'll often want Elm Decoders and Encoders for your types anyway,
-- | we define the classes in those terms, rather than in terms of `Foreign`
-- | directly. Of course, converting from things which handle `Foreign` values
-- | to a decoder or encoder is pretty easy.

module Elm.Port
    ( class PortEncoder, encoder, defaultEncoder
    , class PortEncoderFields, portEncoderFields
    , class PortDecoder, decoder, defaultDecoder
    , class PortDecoderFields, portDecoderFields
    , fromPort, toPort
    ) where

import Data.Foreign (Foreign, toForeign)
import Data.Foreign.Class (class Decode, class Encode, decode)
import Data.Foreign.Class (encode) as Data.Foreign.Class
import Data.List (List(..)) as Data.List
import Data.List (List, (:))
import Data.Maybe (Maybe, maybe)
import Data.Record (insert)
import Data.Record.Unsafe (unsafeGet)
import Data.Sequence (Seq)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Elm.Json.Decode (Decoder, decodeValue, field, fromForeign, succeed)
import Elm.Json.Decode (array, list, maybe, tuple2, tuple3, tuple4, tuple5, tuple6, tuple7, tuple8, unfoldable) as Json.Decode
import Elm.Json.Encode (Value, object)
import Elm.Json.Encode (array, list, null) as Json.Encode
import Elm.Result (Result)
import Prelude (Unit, bind, const, map, pure, unit, ($), (<<<))
import Type.Prelude (class IsSymbol, class RowLacks, class RowToList, SProxy(SProxy), reflectSymbol)
import Type.Row (Cons, Nil, RLProxy(RLProxy), kind RowList)


-- | A class for types which can be decoded when arriving via a `port` or at
-- | app startup via `flags`.
-- |
-- | Instances are provided for all the types that Elm can handle via ports,
-- | except for records. (An instance for records may be possible when we
-- | switch to Purescript 0.12).
-- |
-- | If you want to pass in other values via ports or flags, the usual strategy
-- | in Elm would be to declare a `Value` and then run a decoder on it
-- | yourself.  That would work here as well, or you can write your own
-- | `PortDecoder` instance if you like.
class PortDecoder a where
    decoder :: Decoder a


-- | For types that have a `Decode` instance, you can create an equivalent
-- | `PortDecoder` instance in this way (this one is provided for you):
-- |
-- |     instance stringPortDecoder :: PortDecoder String where
-- |         decoder = defaultDecoder
-- |
-- | We can't make this totally automatic because, even with instance chains,
-- | it would lead to overlapping instances in cases where we want to do
-- | something different than the representation that `Decode` assumes. But, we
-- | can make it easy!
defaultDecoder :: ∀ a. Decode a => Decoder a
defaultDecoder = fromForeign decode


-- | Decode a value coming from a port, or flags.
fromPort :: ∀ a. PortDecoder a => Value -> Result String a
fromPort = decodeValue decoder


-- | A class for types which can be encoded to send to Javascript via ports.
-- |
-- | Instances are provided for all the types that Elm can handle via ports,
-- | except for records. (An instance for records may be possible when we
-- | switch to Purescript 0.12).
-- |
-- | If you want to send other values out via ports, the usual strategy in Elm
-- | would be to declare a `Value` and then run an encoder on it yourself.
-- | That would work here as well, or you can write your own `PortEncoder`
-- | instance if you like.
class PortEncoder a where
    encoder :: a -> Value


-- | For types that have an `Encode` instance, you can create an equivalent
-- | `PortEncoder` instance in this way (this one is provided for you):
-- |
-- |     instance stringPortEncoder :: PortEncoder String where
-- |         encoder = defaultEncoder
-- |
-- | We can't make this totally automatic because, even with instance chains,
-- | it would lead to overlapping instances in cases where we want to do
-- | something different than the representation that `Encode` provides. But,
-- | we can make it easy!
defaultEncoder :: ∀ a. Encode a => a -> Value
defaultEncoder = Data.Foreign.Class.encode


-- | Encode a value to send out a port.
toPort :: ∀ a. PortEncoder a => a -> Value
toPort = encoder


-- I suppose we could check for a `{}` representation, but it seems reasonable
-- to always succeed.
instance unitPortDecoder :: PortDecoder Unit where
    decoder = succeed unit

-- TODO: Check what Elm does for this! I think it probably encodes as an
-- empty array, since Elm conceives of this as a Tuple0.
instance unitPortEncoder :: PortEncoder Unit where
    encoder = const $ Json.Encode.array []


instance booleanPortDecoder :: PortDecoder Boolean where
    decoder = defaultDecoder

instance booleanPortEncoder :: PortEncoder Boolean where
    encoder = defaultEncoder


instance stringPortDecoder :: PortDecoder String where
    decoder = defaultDecoder

instance stringPortEncoder :: PortEncoder String where
    encoder = defaultEncoder


instance intPortDecoder :: PortDecoder Int where
    decoder = defaultDecoder

instance intPortEncoder :: PortEncoder Int where
    encoder = defaultEncoder


instance numberPortDecoder :: PortDecoder Number where
    decoder = defaultDecoder

instance numberPortEncoder :: PortEncoder Number where
    encoder = defaultEncoder


instance listPortDecoder :: PortDecoder a => PortDecoder (List a) where
    decoder = Json.Decode.list decoder

instance listPortEncoder :: PortEncoder a => PortEncoder (List a) where
    encoder = Json.Encode.list <<< map encoder


instance seqPortDecoder :: PortDecoder a => PortDecoder (Seq a) where
    decoder = Json.Decode.array decoder

instance seqPortEncoder :: PortEncoder a => PortEncoder (Seq a) where
    encoder = toForeign <<< map encoder


instance arrayDecoder :: PortDecoder a => PortDecoder (Array a) where
    decoder = Json.Decode.unfoldable decoder

instance arrayEncoder :: PortEncoder a => PortEncoder (Array a) where
    encoder = toForeign <<< map encoder


-- There must be a more general way to do tuples, but I'm not seeing it yet.
-- For now, we're relying on overlapping instances, so the lexical ordering
-- is important. In Purscript 0.12, we'll be able to use instance chains.

instance tupleABCDEFGHPortEncoder :: (PortEncoder a, PortEncoder b, PortEncoder c, PortEncoder d, PortEncoder e, PortEncoder f, PortEncoder g, PortEncoder h) => PortEncoder (a /\ b /\ c /\ d /\ e /\ f /\ g /\ h) where
    encoder (a /\ b /\ c /\ d /\ e /\ f /\ g /\ h) =
        Json.Encode.array
            [ encoder a
            , encoder b
            , encoder c
            , encoder d
            , encoder e
            , encoder f
            , encoder g
            , encoder h
            ]

instance tupleABCDEFGPortEncoder :: (PortEncoder a, PortEncoder b, PortEncoder c, PortEncoder d, PortEncoder e, PortEncoder f, PortEncoder g) => PortEncoder (a /\ b /\ c /\ d /\ e /\ f /\ g) where
    encoder (a /\ b /\ c /\ d /\ e /\ f /\ g) =
        Json.Encode.array
            [ encoder a
            , encoder b
            , encoder c
            , encoder d
            , encoder e
            , encoder f
            , encoder g
            ]

instance tupleABCDEFPortEncoder :: (PortEncoder a, PortEncoder b, PortEncoder c, PortEncoder d, PortEncoder e, PortEncoder f) => PortEncoder (a /\ b /\ c /\ d /\ e /\ f) where
    encoder (a /\ b /\ c /\ d /\ e /\ f) =
        Json.Encode.array
            [ encoder a
            , encoder b
            , encoder c
            , encoder d
            , encoder e
            , encoder f
            ]

instance tupleABCDEPortEncoder :: (PortEncoder a, PortEncoder b, PortEncoder c, PortEncoder d, PortEncoder e) => PortEncoder (a /\ b /\ c /\ d /\ e) where
    encoder (a /\ b /\ c /\ d /\ e) =
        Json.Encode.array
            [ encoder a
            , encoder b
            , encoder c
            , encoder d
            , encoder e
            ]

instance tupleABCDPortEncoder :: (PortEncoder a, PortEncoder b, PortEncoder c, PortEncoder d) => PortEncoder (a /\ b /\ c /\ d) where
    encoder (a /\ b /\ c /\ d) =
        Json.Encode.array
            [ encoder a
            , encoder b
            , encoder c
            , encoder d
            ]

instance tupleABCPortEncoder :: (PortEncoder a, PortEncoder b, PortEncoder c) => PortEncoder (a /\ b /\ c) where
    encoder (a /\ b /\ c) =
        Json.Encode.array
            [ encoder a
            , encoder b
            , encoder c
            ]

instance tupleABPortEncoder :: (PortEncoder a, PortEncoder b) => PortEncoder (a /\ b) where
    encoder (a /\ b) =
        Json.Encode.array
            [ encoder a
            , encoder b
            ]


instance tupleABCDEFGHPortDecoder :: (PortDecoder a, PortDecoder b, PortDecoder c, PortDecoder d, PortDecoder e, PortDecoder f, PortDecoder g, PortDecoder h) => PortDecoder (a /\ b /\ c /\ d /\ e /\ f /\ g /\ h) where
    decoder =
        Json.Decode.tuple8 (\a b c d e f g h -> a /\ b /\ c /\ d /\ e /\ f /\ g /\ h) decoder decoder decoder decoder decoder decoder decoder decoder

instance tupleABCDEFGPortDecoder :: (PortDecoder a, PortDecoder b, PortDecoder c, PortDecoder d, PortDecoder e, PortDecoder f, PortDecoder g) => PortDecoder (a /\ b /\ c /\ d /\ e /\ f /\ g) where
    decoder =
        Json.Decode.tuple7 (\a b c d e f g -> a /\ b /\ c /\ d /\ e /\ f /\ g) decoder decoder decoder decoder decoder decoder decoder

instance tupleABCDEFPortDecoder :: (PortDecoder a, PortDecoder b, PortDecoder c, PortDecoder d, PortDecoder e, PortDecoder f) => PortDecoder (a /\ b /\ c /\ d /\ e /\ f) where
    decoder =
        Json.Decode.tuple6 (\a b c d e f -> a /\ b /\ c /\ d /\ e /\ f) decoder decoder decoder decoder decoder decoder

instance tupleABCDEPortDecoder :: (PortDecoder a, PortDecoder b, PortDecoder c, PortDecoder d, PortDecoder e) => PortDecoder (a /\ b /\ c /\ d /\ e) where
    decoder =
        Json.Decode.tuple5 (\a b c d e -> a /\ b /\ c /\ d /\ e) decoder decoder decoder decoder decoder

instance tupleABCDPortDecoder :: (PortDecoder a, PortDecoder b, PortDecoder c, PortDecoder d) => PortDecoder (a /\ b /\ c /\ d) where
    decoder =
        Json.Decode.tuple4 (\a b c d -> a /\ b /\ c /\ d) decoder decoder decoder decoder

instance tupleABCPortDecoder :: (PortDecoder a, PortDecoder b, PortDecoder c) => PortDecoder (a /\ b /\ c) where
    decoder =
        Json.Decode.tuple3 (\a b c -> a /\ b /\ c) decoder decoder decoder

instance tupleABPortDecoder :: (PortDecoder a, PortDecoder b) => PortDecoder (a /\ b) where
    decoder =
        Json.Decode.tuple2 Tuple decoder decoder


instance maybePortDecoder :: PortDecoder a => PortDecoder (Maybe a) where
    decoder = Json.Decode.maybe decoder

instance maybePortEncoder :: PortEncoder a => PortEncoder (Maybe a) where
    encoder = maybe Json.Encode.null encoder


instance foreignPortDecoder :: PortDecoder Foreign where
    decoder = defaultDecoder

instance foreignPortEncoder :: PortEncoder Foreign where
    encoder = defaultEncoder


class PortEncoderFields (list :: RowList) (row :: # Type) where
    portEncoderFields :: RLProxy list -> Record row -> List (String /\ Value)

instance portEncoderFieldsNil :: PortEncoderFields Nil row where
    portEncoderFields _ _ = Data.List.Nil

instance portEncoderFieldsCons ::
    ( IsSymbol key
    , PortEncoderFields tail row
    , PortEncoder focus
    ) =>
    PortEncoderFields (Cons key focus tail) row where
        portEncoderFields _ record =
            (key /\ encoder focus) : tail
            where
                key = reflectSymbol (SProxy :: SProxy key)
                focus = unsafeGet key record :: focus
                tail = portEncoderFields (RLProxy :: RLProxy tail) record


instance portEncoderRecord ::
    ( RowToList row list
    , PortEncoderFields list row
    ) =>
    PortEncoder (Record row) where
        encoder record =
            object $ portEncoderFields (RLProxy :: RLProxy list) record


instance portDecoderRecord ::
    ( RowToList row list
    , PortDecoderFields list row
    ) =>
    PortDecoder (Record row) where
        decoder =
            portDecoderFields (RLProxy :: RLProxy list)


class PortDecoderFields (list :: RowList) (row :: # Type) | list -> row where
    portDecoderFields :: RLProxy list -> Decoder (Record row)

instance portDecoderFieldsNil :: PortDecoderFields Nil () where
    portDecoderFields _ = pure {}

instance portDecoderFieldsCons ::
    ( PortDecoder focus
    , PortDecoderFields listRest rowRest
    , RowLacks key rowRest
    , RowCons key focus rowRest rowFull
    , RowToList rowFull (Cons key focus listRest)
    , IsSymbol key
    ) =>
    PortDecoderFields (Cons key focus listRest) rowFull where
        portDecoderFields _ = do
            let key = reflectSymbol (SProxy :: SProxy key)
            field <- field key decoder
            rec <- portDecoderFields (RLProxy :: RLProxy listRest)
            pure $ insert (SProxy :: SProxy key) field rec
