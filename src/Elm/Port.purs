
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
    ( class PortEncoder, decoder, defaultDecoder
    , class PortDecoder, encoder, defaultEncoder
    , fromPort, toPort
    ) where

import Data.Foreign (Foreign, toForeign)
import Data.Foreign.Class (class Decode, class Encode, decode)
import Data.Foreign.Class (encode) as Data.Foreign.Class
import Data.List (List)
import Data.Maybe (Maybe, maybe)
import Data.Sequence (Seq)
import Data.Tuple.Nested (type (/\), (/\))
import Elm.Json.Decode (Decoder, decodeValue, fromForeign, succeed)
import Elm.Json.Decode (array, list, maybe, unfoldable) as Json.Decode
import Elm.Json.Encode (Value)
import Elm.Json.Encode (array, list, null) as Json.Encode
import Elm.Result (Result)
import Prelude (Unit, const, map, unit, ($), (<<<))


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


instance tuple2PortEncoder :: (PortEncoder a, PortEncoder b) => PortEncoder (a /\ b) where
    encoder (a /\ b) =
        Json.Encode.array
            [ encoder a
            , encoder b
            ]

instance tuple3PortEncoder :: (PortEncoder a, PortEncoder b, PortEncoder c) => PortEncoder (a /\ b /\ c) where
    encoder (a /\ b /\ c) =
        Json.Encode.array
            [ encoder a
            , encoder b
            , encoder c
            ]


-- TODO: Once we switch to 0.12, instances for Records.


instance maybePortDecoder :: PortDecoder a => PortDecoder (Maybe a) where
    decoder = Json.Decode.maybe decoder

instance maybePortEncoder :: PortEncoder a => PortEncoder (Maybe a) where
    encoder = maybe Json.Encode.null encoder


instance foreignPortDecoder :: PortDecoder Foreign where
    decoder = defaultDecoder

instance foreignPortEncoder :: PortEncoder Foreign where
    encoder = defaultEncoder
