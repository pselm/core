
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
    ( class PortEncoder, decoder
    , class PortDecoder, encoder  
    ) where

import Elm.Json.Decode (Decoder)
import Elm.Json.Encode (Value)


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
