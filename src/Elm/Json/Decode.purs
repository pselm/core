
-- | > Turn JSON values into Elm values. Definitely check out this [intro to
-- | > JSON decoders][guide] to get a feel for how this library works!
-- |
-- | [guide]: https://guide.elm-lang.org/interop/json.html
-- |
-- | Elm's `Json.Decode` doesn't seem to be quite like any existing Purescript
-- | package, so I've re-implemented it, using parts of purescript-foreign as
-- | a base. For other approaches to decoding JSON in Purescript, you could see
-- | purescript-foreign, and the purescript-argonaut packages. It would
-- | probasbly be a good idea to extend this code by allowing for integration
-- | with purescript-argonaut as an option in addition to purescript-foreign.
-- |
-- | The key difference between Elm's approach to decoders and what
-- | purescript-foreign or purescript-argonaut do is that Elm needs to be
-- | able to make some kind of decision about the equality of two decoders, in
-- | order for the virtual DOM to decide whether a listener can be kept or
-- | needs to be removed and re-added.  This drives the design of this module
-- | towards a kind of DSL that allows `equalDecoders` to do a little bit more
-- | than just compare decoders for referential equality, at least in some cases.
-- | I've documented how the various functions in this module interact with
-- | `equalDecoders`.
-- |
-- | Note that (so far) we're not trying to preserve all the nice error messages
-- | that Elm gives ... we could do a better job of that. (Elm uses a different
-- | approach to JSON errors in Elm 0.19, so I may well wait for that in order
-- | to do something nicer with errors here).

module Elm.Json.Decode
    ( module Virtual
    , Decoder
    , decodeString, decodeValue
    , fromForeign
    , string, int, float, bool, null, null_
    , list, array, unfoldable
    , tuple1, tuple2, tuple3, tuple4, tuple5, tuple6, tuple7, tuple8
    , field, (:=), at, index
    , object1, object2, object3, object4, object5, object6, object7, object8
    , keyValuePairs, dict
    , nullable, maybe, fail, succeed, succeed_
    , value, customDecoder, lazy
    , equalDecoders, equalDecoders_
    ) where


import Control.Alt (class Alt, (<|>))
import Control.Apply (lift2, lift3, lift4, lift5)
import Control.Bind ((>=>))
import Control.Monad.Except (runExcept)
import Control.Plus (class Plus)
import Data.Array as Array
import Data.Coyoneda (Coyoneda, coyoneda, unCoyoneda)
import Data.Either (Either(..))
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (class Foldable, foldMap)
import Data.Foldable (oneOf) as Virtual
import Data.Foreign (Foreign, ForeignError(..), F, readArray, readString, readBoolean, readNumber, readInt, isNull, isUndefined, typeOf)
import Data.Foreign as DF
import Data.Foreign.Index (readIndex, readProp)
import Data.Foreign.JSON (parseJSON)
import Data.Leibniz (type (~), Leibniz(Leibniz), coerce, coerceSymm, symm)
import Data.Monoid (class Monoid)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Elm.Apply (andMap, map2, map3, map4, map5, map6, map7, map8) as Virtual
import Elm.Apply (map6, map7, map8)
import Elm.Array as Elm.Array
import Elm.Basics (Bool, Float)
import Elm.Bind (andThen) as Virtual
import Elm.Dict (Dict)
import Elm.Dict as Dict
import Elm.Json.Encode (Value)
import Elm.Json.Encode (Value) as Virtual
import Elm.List (List, foldr)
import Elm.Maybe (Maybe(..))
import Elm.Result (Result(Err, Ok))
import Prelude (class Applicative, class Apply, class Bind, class Eq, class Functor, class Monad, Unit, apply, bind, const, discard, eq, id, map, pure, show, unit, void, (#), ($), (&&), (+), (<#>), (<$>), (<<<), (<>), (==), (>>=), (>>>), (||))
import Prelude (map) as Virtual
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (reallyUnsafeRefEq)


-- | > A value that knows how to decode JSON values.
-- |
-- | Here are some notes about the degree to which the instances preserve the
-- | ability of `equalDecoders` to detect equality. See the docs for
-- | `equalDecoders` for a more general explanation -- it can always detect the
-- | equality of decoders that are the very same decoder (i.e. with the same
-- | reference), so the question is how well it can deteect the equality of
-- | decoders that are separately constructed (not the very same reference).
-- |
-- | The `Functor` instance preserves equality detection so long as the
-- | function you supply to `map` is referentially equal in each case (since we
-- | can't check functions for equality except via referential equality). So,
-- | it is better to `map` with a function that you've pulled out to the
-- | top-level, with a stable reference, rather than a function defined inline
-- | as a lambda. Of course, the decoder you supply to `map` must also itself
-- | have preserved equality detection.
-- |
-- | The `Alt` instance preserves equality detection. Of course, the decoders
-- | you supply to `alt` or `<|>` must themselves have been constructed in a
-- | way that preserves equality detection.
-- |
-- | The `Plus` instance preserves equality detection. (I.e. one `empty` decoder
-- | will be equal to another, and should interact with `<|>` and `oneOf` in
-- | the correct ways).
-- |
-- | The `Apply` instance preserves equality detection. Thus, the results of
-- } using `<*>` and `map2` through `map8` should work with `equalDecoders`
-- | (to the extent that the inputs did).
-- |
-- | The `Applicative` instance isn't able to insist on an `Eq` constraint for
-- | the value you supply to `pure`. Thus, `equalDecoders` will be limited to
-- | using referential equality on values you supply to `pure`. So, it will be
-- | preferable to use `succeed` directly where you can.
-- |
-- | The `Bind` instance preserves equality detection if the supplied function
-- | is referentially equal in each case. This is a little awkward for
-- | do-notation, or an `|> andThen` pipeline (the roughly-equivalent Elm
-- | idiom), since in those cases the functions will typically be defined
-- | inline, and thus not with a stable reference. However, if you can stick to
-- | functions with stable references, rather than defined inline, (>>=) and
-- | `andThen` will preserve equality.
--
-- We collect some evidence about what the `a` is, in cases where it's not
-- fully polymorphic.
--
-- The `Run` constructor is probably an example of something more general that
-- I'm not seeing yet.
--
-- This little DSL could probably be sipmlified a bit ...  ideally, it should
-- consist of irreducible things. It's also possible that we should be using a
-- "free monad" here ... this is reminiscent of a free monad, but I couldn't
-- quite figure out how to literally use one and still get the Elm API. (E.g.
-- it seemed that the `Decoder` would need an extra type variable, which
-- perhaps could be elimimated in some way, but I didn't get far enough down
-- that road to figure it out).
data Decoder a
    = Ap (ApplyCoyoneda Decoder a)
    | Array (a ~ Array Foreign)
    | ArrayOfLength (a ~ Array Foreign) Int
    | Bind (BindCoyoneda Decoder a)
    | Empty
    | Fail String
    | Field (a ~ Foreign) String
    | FromForeign (Foreign -> F a)
    | Index (a ~ Foreign) Int
    | Keys (a ~ Array String)
    | Map (Coyoneda Decoder a)
    | Null (Maybe (a -> a -> Bool)) a
    | OneOf (Decoder a) (Decoder a)
    | Run (Decoder Foreign) (Decoder a)
    | RunArray (∀ r. (∀ f x. (a ~ f x) -> RunArray f x -> r) -> r)
    | Succeed (Maybe (a -> a -> Bool)) a
    | Value (a ~ Foreign)


-- For doing further work on arrays, we collect some input, a decoder, and
-- an `unfoldr` function, so we can produce any collection type that has
-- an `Unfoldable` instance.
type RunArray f x =
    { input :: Decoder (Array Foreign)
    , tagger :: Decoder x
    , unfoldr :: (Int -> Maybe (Tuple x Int)) -> Int -> f x
    }


-- | This is for a case where I think I've got good Leibniz evidence, but
-- | `lowerLeibniz` doesn't do quite what I need. What I've got is:
-- |
-- |     f0 x1 ~ f2 x3
-- |
-- | However, what I need is:
-- |
-- |     x1 ~ x3
-- |
-- | So, this is like `lowerLeibniz`, but `lowerLeibniz` requires a match
-- | between the `f0` and the `f2`. But I don't entirely see why. If I can prove
-- | that `f a ~ g b`, then doesn't it follow that `a ~ b`?  How could it be
-- | otherwise?
-- |
-- | Haskell's [Leibniz package][leibniz] also requires a `f a ~ f b`, so
-- | perhaps I'm missing something.
-- |
-- | [leibniz]: https://hackage.haskell.org/package/eq-4.2/docs/Data-Eq-Type.html
-- |
-- | However, the GADT version of this in [`Data.Type.Equality`][gadt] has an `inner`
-- | function that does what I need. (As well as the equivalent `outer`).
-- |
-- | [gadt]: https://hackage.haskell.org/package/base-4.10.1.0/docs/Data-Type-Equality.html
-- |
-- | So, I should suggest these for `Data.Leibniz` and see what people think.
inner :: ∀ f g a b. (f a ~ g b) -> (a ~ b)
inner _ = Leibniz unsafeCoerce


-- | Like `inner`, but to witness the equality of the container types. The
-- | Haskell version uses polykinds -- without polykinds, we lift over an
-- | arbitrary `c` type.
outer :: ∀ f g a b c. (f a ~ g b) -> (f c ~ g c)
outer _ = Leibniz unsafeCoerce


-- The Foreign module uses `Except` to pass errors around, whereas we need
-- `Result String a`.
--
-- We should render the err case a little more nicely -- but Elm 0.19 will have
-- some improvements there, so we may as well wait for that.
toResult :: ∀ a. F a -> Result String a
toResult f =
    case runExcept f of
        Right a -> Ok a
        Left err -> Err (show err)


-- | > Run a `Decoder` on some JSON `Value`. You can send these JSON values
-- | > through ports, so that is probably the main time you would use this function.
decodeValue :: ∀ a. Decoder a -> Value -> Result String a
decodeValue =
    -- This gets called recursively without tail-calls, so it will use stack
    -- space. However, a decoder should not be constructed in a way that chews
    -- through too much stack space. If it turns out to be a problem, we can
    -- make this stack-safe in one way or another.
    --
    -- In principle, it might be nice to make this partly lazy ... so that we
    -- can take a Decoder and produce the `Value -> Result String a` function
    -- once, rather than many times. Or, perhaps we could pre-construct the
    -- `Value -> Result String a` function for each decoder and store it in the
    -- type, along with the things we need for `equalDecoders`. Anyway, we can
    -- explore those kinds of strategies at some point.
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

        Empty ->
            -- This case exists to make the `Plus` instance work ... in the
            -- ordinary course, we wouldn't expect to get here, but we'll just
            -- fail if we do.
            const $ Err "Reached an empty decoder"

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

        OneOf Empty right ->
            decodeValue right

        OneOf left Empty ->
            decodeValue left

        OneOf left right ->
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

        RunArray runArray ->
            runArray \proof run ->
                \val -> do
                    decoded <-
                        decodeValue run.input val
                            >>= traverse (decodeValue run.tagger)

                    pure $ coerceSymm proof $
                        run.unfoldr (\i ->
                            (\a -> Tuple a (i + 1)) <$> Array.index decoded i
                        ) 0

        Succeed _ a ->
            const $ Ok a

        Value proof ->
            id >>> coerceSymm proof >>> Ok


-- | `equalDecoders` attempts to compare two decoders for equality. It is
-- | subject to false negatives, but positives should be reliable. (For this
-- | reason, we don't provide an `Eq` instance for `Decoder` ... this is a
-- | function that has a specialized use, rather than being a fully reliable
-- | test for equality).
-- |
-- | This is roughly equivalent to a function that Elm uses internally (not
-- | exposed in Elm) as part of the virtual DOM, to decide whether a listener
-- | must be removed and re-applied (because it uses a different decoder than
-- | the previously applied listener). So, false negatives are an efficiency
-- | issue (as listeners will be removed and re-applied unnecessarily), while
-- | false positives would be a more serious problem (and should not occur).
-- |
-- | I have documented, for each function in the module, how well it preserves
-- | the ability of `equalDecoders` to detect equality. The cases fall roughly
-- | into these categories. (Elm's behaviour with respect to detecting equality
-- | is roughly similar, I believe).
-- |
-- | ### The very same decoder
-- |
-- | If two decoders are the very same thing (i.e. referentially equal), then
-- | `equalDecoders` will reliably detect that. So, if your `view` code
-- | references a decoder by its top=level name, then `equalDecoders` will
-- | detect that the decoder is equal to itself, on the next round. The decoder
-- | may have been constructed in whatever complex way is necessary, but if you
-- | refer to it via its top-level name (not a function call), then
-- | `equalDecoders` will work well with it. So, in cases where it is possible
-- | to define your decoder at the top-level, that is handy.
-- |
-- | So, if you define a decoder like this:
-- |
-- |     decodePerson :: Decoder Person
-- |     decodePerson =
-- |        ...
-- |
-- | ... that is, as a value, without arguments, then it doesn't matter what
-- | you do in the `...` to construct the decoder ... `equalDecoders` will be
-- | able to detect that `decodePerson` is equal to `decodePerson`.
-- |
-- | For a decoder to take advantage of this, it must be defined without taking
-- | arguments, and must be defined at the top-level (i.e. not inside a `let`
-- | expression). Otherwise, the decoder won't have a stable reference. It may
-- | still compare successfully with `equalDecoders`, but that will depend on
-- | exactly how it is constructed. If the decoder has a stable reference, then
-- | it doesn't matter how it was constructed.
-- |
-- | ### Not the very same decoder
-- |
-- | If two decoders are not the very same thing, then whether `equalDecoders`
-- | can successfully detect equality depends on how they were constructed and
-- | combined. I've documented the effect of each function in this module on
-- | the detection of equality, but the general rules are as follows:
-- |
-- | - The equality of primitive decoders can always be detected
-- |
-- |   e.g. `float`, `int`, `bool`, `string`, `value`
-- |
-- | - If you have to supply an argument that is a function, then we can only
-- |   detect equality if you supply a function that is referentially equal in
-- |   each case. So, it's better to avoid defining functions "inline" as a
-- |   lambda when creating a decoder. Instead, try to pull the functions out
-- |   to the top-level where you can, so they will have stable references for
-- |   the purpose of testing referential equality.
-- |
-- |   e.g. `map`, `andThen`, `bind`, `fromForeign`, `customDecoder`, `lazy`
-- |
-- |   Note that for `bind`, this means that we'll have a limited ability to
-- |   detect the equality of decoders defined using `do` notation (or an `|>
-- |   andThen` pipeline, in the equivalent Elm idiom). Since the repeated
-- |   "binds" are defined inline, they won't have stable references. However,
-- |   if you can pull all but the first `bind` out into a stable reference,
-- |   then a single `>>=` (or `andThen`) which refers to the stable reference
-- |   will preserve equality.
-- |
-- | - If you have to supply other decoders as arguments, then generally we
-- |   preserve equality detection. That is, the resulting decoder will generally
-- |   work as well with `equalDecoders` as the decoders you supply.
-- |
-- |   e.g. `alt`, `<|>`, `oneOf`, `field`, `at`, `index`, `field`, `list`, `array`,
-- |        `unfoldable`, `nullable`, `maybe`
-- |
-- | - If you supply values as an argument, then `equalDecoders` works best if
-- |   you use functions that require an `Eq` instance. In those cases, we can
-- |   use the `Eq` instance to compare the values when detecting the equality
-- |   of decoders. Otherwise, we have to fall back on referential equality.
-- |   So, prefer `succeed` and `null` to `succeed_` and `null_`.
-- |
-- |   One way in which this is a little awkward is that a "bare" record type
-- |   cannot have an `Eq` instance -- you will need to make a `newtype` for
-- |   it. However, once you've done that, the compiler can often derive an
-- |   `Eq` instance for you (along with providing various other
-- |   newtype-related conveniences), so it is only a mild nuisance. (Elm
-- |   instead has a magic `==` that works with bare record types, though not
-- |   without its own difficulties -- there is no free lunch here).
-- |
-- | - There are some functions which currently destroy the ability to detect
-- |   equality (unless you keep a stable reference to the result), but which
-- |   should be fixable.
-- |
-- |   e.g. `keyValuePairs`, `dict`, and `tuple1` through `tuple8`
equalDecoders :: ∀ a. Decoder a -> Decoder a -> Bool
equalDecoders = equalDecodersImpl (Just id)


-- | Like `equalDecoders`, but doesn't rely on the decoders being of the same
-- | type. If you know the decoders are of the same type, `equalDecoders` can
-- | do a somewhat better job of determining equality.
equalDecoders_ :: ∀ a b. Decoder a -> Decoder b -> Bool
equalDecoders_ = equalDecodersImpl Nothing


-- | We can do slightly different things depending on whethr we'd got evidence
-- | that the two decoders are of the same type. So, the first parameter
-- | indicates whether we've got that evidence or not.
equalDecodersImpl :: ∀ a b. Maybe (a ~ b) -> Decoder a -> Decoder b -> Bool
equalDecodersImpl proof d1 d2 =
    reallyUnsafeRefEq d1 d2 || case d1, d2 of
        Ap coyoLeft, Ap coyoRight ->
            coyoLeft # unApplyCoyoneda (\tagger1 decoder1 ->
            coyoRight # unApplyCoyoneda (\tagger2 decoder2 ->
                -- In this case, we've got decoders on both sides, and we don't
                -- know what type we're coming from. So, the best we can do (at
                -- least for now) is check referential equality of each side.
                -- If either side has referntial equality, then we know the
                -- types match.
                --
                -- Perhaps there would be a way to collect evidence about what
                -- type we're coming from, so that we could compare that
                -- evidence to know whether they are the same type?
                case reallyUnsafeRefEq tagger1 tagger2, reallyUnsafeRefEq decoder1 decoder2 of
                    true, true ->
                         -- If they are both referentially equal, then we're
                         -- equal
                         true

                    false, false ->
                        -- If neither are referntially equal, we don't have a
                        -- witness to the fact that we're coming from the same
                        -- type. So, we can continue, but we can't provide any
                        -- proof.
                        equalDecodersImpl Nothing tagger1 tagger2 &&
                        equalDecodersImpl Nothing decoder1 decoder2

                    true, false ->
                        -- If the taggers are equal, at least we have the same
                        -- types, so we can try on the decoders. I think I have
                        -- to fake the evidence.
                        equalDecodersImpl (Just $ Leibniz unsafeCoerce) decoder1 decoder2

                    false, true ->
                        -- If the decoders are equal, we have the same type, so
                        -- try on the taggers. I think I have to fake the evidence.
                        equalDecodersImpl (Just $ Leibniz unsafeCoerce) tagger1 tagger2
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
                    -- decoder2 have the same type. But I don't think this maps
                    -- to Leibniz very well, so we manufacture the evidence.
                    equalDecodersImpl (Just $ Leibniz unsafeCoerce) decoder1 decoder2
                else
                    false
            ))

        Empty, Empty ->
            true

        Fail reasonLeft, Fail reasonRight ->
            reasonLeft == reasonRight

        Field _ keyLeft, Field _ keyRight ->
            keyLeft == keyRight

        FromForeign funcLeft, FromForeign funcRight ->
            reallyUnsafeRefEq funcLeft funcRight

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
                    equalDecodersImpl (Just $ Leibniz unsafeCoerce) decoder1 decoder2
                else
                    false
            ))

        Null leftEq leftVal, Null rightEq rightVal ->
            case proof, leftEq, leftVal, rightEq, rightVal of
                Just p, Just equals, a, _, b ->
                    -- Same type, and an equals, so we can use it
                    reallyUnsafeRefEq a b || equals a (coerceSymm p b)

                Just p, _, a, Just equals, b ->
                    -- Same type, and other equals, so use it
                    reallyUnsafeRefEq a b || equals (coerce p a) b

                _, Just aEquals, a, Just bEquals, b ->
                    -- If we have an equals function on both sides, and they
                    -- are equal, then clearly we can use it, even if we have
                    -- no other evidence.
                    if reallyUnsafeRefEq aEquals bEquals then
                        reallyUnsafeRefEq a b || aEquals a (unsafeCoerce b)
                    else
                        reallyUnsafeRefEq a b

                _ , _ , a, _, b ->
                    -- Either not the same type, or no ewuals
                    reallyUnsafeRefEq a b

        -- OneOf will work with whichever decoder is not empty, so we check for
        -- that.
        OneOf left1 left2, OneOf right1 right2 ->
            -- Simplifies exhaustivity checking for the compiler
            case left1, left2, right1, right2 of
                Empty, a, b, Empty ->
                    equalDecodersImpl proof a b

                a, Empty, Empty, b ->
                    equalDecodersImpl proof a b

                l1, l2, r1, r2 ->
                    equalDecodersImpl proof l1 r1 &&
                    equalDecodersImpl proof l2 r2

        Run leftLeft leftRight, Run rightLeft rightRight ->
            equalDecodersImpl (Just id) leftLeft rightLeft &&
            equalDecodersImpl proof leftRight rightRight

        RunArray l, RunArray r ->
            -- This one is kind of fun.
            --
            -- Our `leftProof` proves that `a ~ f1 x1` on the left side.
            -- Our `rightProof` proves that `b ~ f2 x2` on the right side.
            -- Our `proof`, if we have it, proves that `a ~ b`.
            --
            -- What we need is proof that  `x ~ y`. To do that, we need to use
            -- our `proof` to show that `f1 x1 ~ f2 x2` (which, of course, only
            -- works if we have our `proof`). Then, we can use a custom `inner`
            -- function to prove `x1 ~ x2` ... it's like `lowerLeibniz`, but
            -- doesn't require the type constructors to be the same. (Which,
            -- logically, doesn't seem necessary).
            l \leftProof left ->
            r \rightProof right ->
                let
                    taggerProof =
                        proof <#> \p ->
                            inner $ symm leftProof >>> p >>> rightProof
                in
                    -- left.input and right.input are necessarily the same type,
                    -- so we can trivially provide the proof.
                    equalDecodersImpl (Just id) left.input right.input &&
                    equalDecodersImpl taggerProof left.tagger right.tagger &&
                    reallyUnsafeRefEq left.unfoldr right.unfoldr

        Succeed leftEq leftVal, Succeed rightEq rightVal ->
            case proof, leftEq, leftVal, rightEq, rightVal of
                Just p, Just equals, a, _, b ->
                    -- Same types, and an equals
                    reallyUnsafeRefEq a b || equals a (coerceSymm p b)

                Just p, _, a, Just equals, b ->
                    -- Same types and the other equals
                    reallyUnsafeRefEq a b || equals (coerce p a) b

                _, Just aEquals, a, Just bEquals, b ->
                    -- If we have an equals function on both sides, and they
                    -- are equal, then clearly we can use it, even if we have
                    -- no other evidence.
                    if reallyUnsafeRefEq aEquals bEquals then
                        reallyUnsafeRefEq a b || aEquals a (unsafeCoerce b)
                    else
                        reallyUnsafeRefEq a b

                _, _, a, _, b ->
                    -- Not the same types, or no ewuals
                    reallyUnsafeRefEq a b

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
        -- We know what the answer will be up-front for certain mapping
        -- operations, so we don't need to defer all of them.
        case decoder of
            Empty ->
                Empty

            Fail err ->
                Fail err

            _ ->
                Map $ coyoneda func decoder


instance altDecoder :: Alt Decoder where
    -- We can apply the `Plus` laws immediately (but we also consider them in
    -- `decodeValue` and `equalDecoders`, just in case).
    alt Empty b = b
    alt a Empty = a
    alt a b = OneOf a b


instance plusDecoder :: Plus Decoder where
    empty = Empty


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
    -- | Works with `equalDecoders` so long ast hte functions supplied are
    -- | referentially equal.
    bind decoder func =
        case decoder of
            Empty ->
                Empty

            Fail err ->
                Fail err

            _ ->
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


-- | > Parse the given string into a JSON value and then run the `Decoder` on it.
-- | > This will fail if the string is not well-formed JSON or if the `Decoder`
-- | > fails for some reason.
-- |
-- | >     decodeString int "4"     == Ok 4
-- | >     decodeString int "1 + 2" == Err ...
decodeString :: ∀ a. Decoder a -> String -> Result String a
decodeString decoder str =
    toResult (parseJSON str) >>= decodeValue decoder


-- OBJECTS

-- | > Decode a nested JSON object, requiring certain fields.
-- | >
-- | >     json = """{ "person": { "name": "tom", "age": 42 } }"""
-- | >
-- | >     decodeString (at ["person", "name"] string) json  == Ok "tom"
-- | >     decodeString (at ["person", "age" ] int   ) json  == Ok "42
-- | >
-- | > This is really just a shorthand for saying things like:
-- | >
-- | >     field "person" (field "name" string) == at ["person","name"] string
-- |
-- | Note that the signature is defined in terms of `Foldable` so that it will
-- | work with `Array` or `List` (among others).
-- |
-- | Preserves equality for `equalDecoders`. The resulting decoder will also be
-- | considered equal with decoders constructed manually with nested `field`
-- | applications, if the field names match.
at :: ∀ f a. (Foldable f) => f String -> Decoder a -> Decoder a
at fields decoder =
    foldr field decoder fields


-- | > Decode a JSON array, requiring a particular index.
-- | >
-- | >     json = """[ "alice", "bob", "chuck" ]"""
-- | >
-- | >     decodeString (index 0 string) json  == Ok "alice"
-- | >     decodeString (index 1 string) json  == Ok "bob"
-- | >     decodeString (index 2 string) json  == Ok "chuck"
-- | >     decodeString (index 3 string) json  == Err ...
-- |
-- | This function was added in Elm 0.18.
-- |
-- | `equalDecoders` will consider the resulting decoder equal to another
-- | produced by this function if the indexes are equal and the supplied
-- | decoders are themselves considered equal by `equalDecoders`. So, it is
-- | equality-preserving.
index :: ∀ a. Int -> Decoder a -> Decoder a
index = Run <<< indexT


-- | > Decode a JSON object, requiring a particular field.
-- | >
-- | >     decodeString (field "x" int) "{ \"x\": 3 }"            == Ok 3
-- | >     decodeString (field "x" int) "{ \"x\": 3, \"y\": 4 }"  == Ok 3
-- | >     decodeString (field "x" int) "{ \"x\": true }"         == Err ...
-- | >     decodeString (field "x" int) "{ \"y\": 4 }"            == Err ...
-- | >
-- | >     decodeString (field "name" string) "{ \"name\": \"tom\" }" == Ok "tom"
-- | >
-- | > The object *can* have other fields. Lots of them! The only thing this decoder
-- | > cares about is if `x` is present and that the value there is an `Int`.
-- | >
-- | > Check out [`map2`](#map2) to see how to decode multiple fields!
-- |
-- | `equalDecoders` will consider the resulting decoder equal to another
-- | produced by this function if the field names are equal and the supplied
-- | decoders are themselves considered equal by `equalDecoders`. So, it is
-- | equality-preserving.
field :: ∀ a. String -> Decoder a -> Decoder a
field = Run <<< fieldT


infixl 4 field as :=


-- | > Apply a function to a decoder.
-- | >
-- | >     object1 sqrt ("x" := float)
-- |
-- | Equivalent to Purescript's `map`.
-- |
-- | Removed in Elm 0.18, in favour of `map`.
-- |
-- | Works with `equalDecoders` so long as the function supplied in one case is
-- | referentially equal to the function supplied in the other.
object1 :: ∀ a value. (a -> value) -> Decoder a -> Decoder value
object1 = map


-- | > Use two different decoders on a JS value. This is nice for extracting
-- | > multiple fields from an object.
-- | >
-- | >     point :: Decoder (Tuple Float Float)
-- | >     point =
-- | >         object2 Tuple
-- | >           ("x" := float)
-- | >           ("y" := float)
-- |
-- | Equivalent to Purescript's `lift2`.
-- |
-- | Removed in Elm 0.18, in favour of `map2`.
-- |
-- | Works with `equalDecoders` so long as the function supplied in one case is
-- | referntially equal to the function supplied in the other case, and the
-- | provided decoders preserve equality.
object2 :: ∀ a b value. (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
object2 = lift2


-- | > Use three different decoders on a JS value. This is nice for extracting
-- | > multiple fields from an object.
-- | >
-- | >     type Job = { name :: String, id :: Int, completed :: Bool }
-- | >
-- | >     job :: Decoder Job
-- | >     job =
-- | >         object3 Job
-- | >           ("name" := string)
-- | >           ("id" := int)
-- | >           ("completed" := bool)
-- |
-- | Equivalent to Purescript's `lift3`.
-- |
-- | Removed in Elm 0.18, in favour of `map3`.
-- |
-- | Works with `equalDecoders` so long as the function supplied in one case is
-- | referntially equal to the function supplied in the other case, and the
-- | provided decoders preserve equality.
object3 :: ∀ a b c value. (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
object3 = lift3


-- | Equivalent to Purescript's `lift4`.
-- |
-- | Removed in Elm 0.18, in favour of `map4`.
-- |
-- | Works with `equalDecoders` so long as the function supplied in one case is
-- | referntially equal to the function supplied in the other case, and the
-- | provided decoders preserve equality.
object4 :: ∀ a b c d value. (a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder value
object4 = lift4


-- | Equivalent to Purescript's `lift5`.
-- |
-- | Removed in Elm 0.18, in favour of `map5`.
-- |
-- | Works with `equalDecoders` so long as the function supplied in one case is
-- | referntially equal to the function supplied in the other case, and the
-- | provided decoders preserve equality.
object5 :: ∀ a b c d e value. (a -> b -> c -> d -> e -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder value
object5 = lift5


-- | Removed in Elm 0.18, in favour of `map6`.
-- |
-- | Works with `equalDecoders` so long as the function supplied in one case is
-- | referntially equal to the function supplied in the other case, and the
-- | provided decoders preserve equality.
object6 :: ∀ a b c d e f value. (a -> b -> c -> d -> e -> f -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder value
object6 = map6


-- | Removed in Elm 0.18, in favour of `map7`.
-- |
-- | Works with `equalDecoders` so long as the function supplied in one case is
-- | referntially equal to the function supplied in the other case, and the
-- | provided decoders preserve equality.
object7 :: ∀ a b c d e f g value. (a -> b -> c -> d -> e -> f -> g -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder value
object7 = map7


-- | Removed in Elm 0.18, in favour of `map8`.
-- |
-- | Works with `equalDecoders` so long as the function supplied in one case is
-- | referntially equal to the function supplied in the other case, and the
-- | provided decoders preserve equality.
object8 :: ∀ a b c d e f g h value. (a -> b -> c -> d -> e -> f -> g -> h -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder value
object8 = map8


-- | > Decode a JSON object into an Elm `List` of pairs.
-- | >
-- | >     decodeString (keyValuePairs int) "{ \"alice\": 42, \"bob\": 99 }"
-- | >       == [("alice", 42), ("bob", 99)]
-- |
-- | The container for the return type is polymorphic in order to accommodate `List` or `Array`, among others.
-- |
-- | Does not work with `equalDecoders` yet, but this should be fixable.
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


-- | > Decode a JSON object into an Elm `Dict`.
-- | >
-- | >     decodeString (dict int) "{ \"alice\": 42, \"bob\": 99 }"
-- | >       == Dict.fromList [("alice", 42), ("bob", 99)]
-- |
-- | Does not work with `equalDecoders` yet, but should be fixable.
dict :: ∀ a. Decoder a -> Decoder (Dict String a)
dict decoder =
    let
        decodePairs :: Decoder (List (Tuple String a))
        decodePairs =
            keyValuePairs decoder
    in
    map Dict.fromList decodePairs


-- | Given a function which reads a `Foreign`, make a decoder.
-- |
-- | Note that this is not in the Elm API.
-- |
-- | Because you are supplying a function, `equalDecoders` will only consider
-- | the resulting decoders equal if the function you supply is referentially
-- | equal to the function you supply in the other case.  So, to preserve
-- | equality, the supplied function should not be a lambda -- it should be a
-- | top-level function definition. See the docs for `equalDecoders` for more
-- | discussion.
fromForeign :: ∀ a. (Foreign -> F a) -> Decoder a
fromForeign = FromForeign


-- | > Decode a JSON string into an Elm `String`.
-- | >
-- | >     decodeString string "true"              == Err ...
-- | >     decodeString string "42"                == Err ...
-- | >     decodeString string "3.14"              == Err ...
-- | >     decodeString string "\"hello\""         == Ok "hello"
-- | >     decodeString string "{ \"hello\": 42 }" == Err ...
-- |
-- | Works with `equalDecoders`
string :: Decoder String
string = fromForeign readString


-- | > Decode a JSON number into an Elm `Float`.
-- | >
-- | >     decodeString float "true"              == Err ..
-- | >     decodeString float "42"                == Ok 42
-- | >     decodeString float "3.14"              == Ok 3.14
-- | >     decodeString float "\"hello\""         == Err ...
-- | >     decodeString float "{ \"hello\": 42 }" == Err ...
-- |
-- | Works with `equalDecoders`
float :: Decoder Float
float = fromForeign readNumber


-- | > Decode a JSON number into an Elm `Int`.
-- | >
-- | >     decodeString int "true"              == Err ...
-- | >     decodeString int "42"                == Ok 42
-- | >     decodeString int "3.14"              == Err ...
-- | >     decodeString int "\"hello\""         == Err ...
-- | >     decodeString int "{ \"hello\": 42 }" == Err ...
-- |
-- | Works with `equalDecoders`
int :: Decoder Int
int = fromForeign readInt


-- | > Decode a JSON boolean into an Elm `Bool`.
-- | >
-- | >     decodeString bool "true"              == Ok True
-- | >     decodeString bool "42"                == Err ...
-- | >     decodeString bool "3.14"              == Err ...
-- | >     decodeString bool "\"hello\""         == Err ...
-- | >     decodeString bool "{ \"hello\": 42 }" == Err ...
-- |
-- | Works with `equalDecoders`
bool :: Decoder Bool
bool = fromForeign readBoolean


-- | > Decode a JSON array into an Elm `List`.
-- | >
-- | >     decodeString (list int) "[1,2,3]"       == Ok [1,2,3]
-- | >     decodeString (list bool) "[true,false]" == Ok [True,False]
-- |
-- | Preserves equality-checking for the input with `equalDecoders`
-- |
-- | You can also use `unfoldable` to decode into any container type that has
-- | an `Unfoldable` instance.
list :: ∀ a. Decoder a -> Decoder (List a)
list = unfoldable


-- | > Decode a JSON array into an Elm `Array`.
-- | >
-- | >     decodeString (array int) "[1,2,3]"       == Ok (Array.fromList [1,2,3])
-- | >     decodeString (array bool) "[true,false]" == Ok (Array.fromList [True,False])
-- |
-- | Preserves equality-checking for the input with `equalDecoders`
-- |
-- | You can also use `unfoldable` to decode into any container type that has
-- | an `Unfoldable` instance.
array :: ∀ a. Decoder a -> Decoder (Elm.Array.Array a)
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
-- |
-- | Preserves equality-checking for the input with `equalDecoders`
unfoldable :: ∀ f a. Unfoldable f => Decoder a -> Decoder (f a)
unfoldable decoder =
    RunArray
        \func ->
            func id
                { input : arrayT
                , tagger : decoder
                , unfoldr : unfoldr
                }

-- | > Decode a `null` value into some Elm value.
-- | >
-- | >     decodeString (null False) "null" == Ok False
-- | >     decodeString (null 42) "null"    == Ok 42
-- | >     decodeString (null 42) "42"      == Err ..
-- | >     decodeString (null 42) "false"   == Err ..
-- | >
-- | > So if you ever see a `null`, this will return whatever value you specified.
-- |
-- | Works with `equalDecoders`
null :: ∀ a. Eq a => a -> Decoder a
null = Null (Just eq)


-- | Like `null`, but for cases where your default value does not have an `Eq`
-- | instance. Use `null` where you can, because it will make `equalDecoders`
-- | more reliable.
null_ :: ∀ a. a -> Decoder a
null_ = Null Nothing


-- | > Decode a nullable JSON value into an Elm value.
-- | >
-- | >     decodeString (nullable int) "13"    == Ok (Just 13)
-- | >     decodeString (nullable int) "42"    == Ok (Just 42)
-- | >     decodeString (nullable int) "null"  == Ok Nothing
-- | >     decodeString (nullable int) "true"  == Err ..
-- |
-- | This function was added in Elm 0.18.
-- |
-- | For `equalDecoders`, this preserves whatever answer would be given for the
-- | inputs.
nullable :: ∀ a. Decoder a -> Decoder (Maybe a)
nullable decoder =
    null_ Nothing <|> map Just decoder


-- | > Helpful for dealing with optional fields. Here are a few slightly different
-- | > examples:
-- | >
-- | >     json = """{ "name": "tom", "age": 42 }"""
-- | >
-- | >     decodeString (maybe (field "age"    int  )) json == Ok (Just 42)
-- | >     decodeString (maybe (field "name"   int  )) json == Ok Nothing
-- | >     decodeString (maybe (field "height" float)) json == Ok Nothing
-- | >
-- | >     decodeString (field "age"    (maybe int  )) json == Ok (Just 42)
-- | >     decodeString (field "name"   (maybe int  )) json == Ok Nothing
-- | >     decodeString (field "height" (maybe float)) json == Err ...
-- | >
-- | > Notice the last example! It is saying we *must* have a field named `height` and
-- | > the content *may* be a float. There is no `height` field, so the decoder fails.
-- | >
-- | > Point is, `maybe` will make exactly what it contains conditional. For optional
-- | > fields, this means you probably want it *outside* a use of `field` or `at`.
-- |
-- | `equalDecoders` will consider the results of this function to be equal if
-- | the provided decoder is equal ... that is, `maybe` is equal-preserving.
maybe :: ∀ a. Decoder a -> Decoder (Maybe a)
maybe decoder =
    map Just decoder <|> succeed_ Nothing


-- | > Do not do anything with a JSON value, just bring it into Elm as a `Value`.
-- | > This can be useful if you have particularly crazy data that you would like to
-- | > deal with later. Or if you are going to send it out a port and do not care
-- | > about its structure.
-- |
-- | Works with `equalDecoders`
value :: Decoder Value
value = valueT


fromResult :: ∀ a. Result String a -> Decoder a
fromResult =
    case _ of
        Ok ok ->
            succeed_ ok

        Err err ->
            fail err


-- | > Create a custom decoder that may do some fancy computation.
-- |
-- | This function was removed in Elm 0.18.
-- |
-- | `equalDecoders` will consider the resulting decoders equal if the input
-- | decoders are equal, and the provided functions in each case are
-- | referentially equal.
customDecoder :: ∀ a b. Decoder a -> (a -> Result String b) -> Decoder b
customDecoder decoder func =
    func <$> decoder >>= fromResult


-- | > Sometimes you have JSON with recursive structure, like nested comments.
-- | > You can use `lazy` to make sure your decoder unrolls lazily.
-- | >
-- | >     type alias Comment =
-- | >       { message : String
-- | >       , responses : Responses
-- | >       }
-- | >
-- | >     type Responses = Responses (List Comment)
-- | >
-- | >     comment : Decoder Comment
-- | >     comment =
-- | >       map2 Comment
-- | >         (field "message" string)
-- | >         (field "responses" (map Responses (list (lazy (\_ -> comment)))))
-- | >
-- | > If we had said `list comment` instead, we would start expanding the value
-- | > infinitely. What is a `comment`? It is a decoder for objects where the
-- | > `responses` field contains comments. What is a `comment` though? Etc.
-- | >
-- | > By using `list (lazy (\_ -> comment))` we make sure the decoder only expands
-- | > to be as deep as the JSON we are given. You can read more about recursive data
-- | > structures [here][].
-- | >
-- | > [here]: https://github.com/elm-lang/elm-compiler/blob/master/hints/recursive-alias.md
-- |
-- | This function was added in Elm 0.18.
-- |
-- | This function works with `equalDecoders` so long as you provide functions
-- | that are referentially equal ... see the docs for `equalDecoders` for more
-- | information.  That's probably the best we can do, since the point of
-- | `lazy` is to avoid unrolling the actual decoder until needed.
lazy :: ∀ a. (Unit -> Decoder a) -> Decoder a
lazy = bind (pure unit)


-- | > Ignore the JSON and make the decoder fail. This is handy when used with
-- | > `oneOf` or `andThen` where you want to give a custom error message in some
-- | > case.
-- |
-- | `equalDecoders` considers two `fail` decoders to be equal if they have the
-- | same message.
fail :: ∀ a. String -> Decoder a
fail = Fail


-- | > Ignore the JSON and produce a certain Elm value.
-- | >
-- | >     decodeString (succeed 42) "true"    == Ok 42
-- | >     decodeString (succeed 42) "[1,2,3]" == Ok 42
-- | >     decodeString (succeed 42) "hello"   == Err ... -- this is not a valid JSON string
-- | >
-- | > This is handy when used with `oneOf` or `andThen`.
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

-- | > Handle an array with exactly one element.
-- | >
-- | >     extractString :: Decoder String
-- | >     extractString =
-- | >         tuple1 identity string
-- | >
-- | >     authorship :: Decoder String
-- | >     authorship =
-- | >         oneOf
-- | >           [ tuple1 (\author -> "Author: " <> author) string
-- | >           , list string |> map (\authors -> "Co-authors: " <> String.join ", " authors)
-- | >           ]
-- |
-- | This function was removed in Elm 0.18.
-- |
-- | Does not work with `equalDecoders` yet, but this is probably fixable.
tuple1 :: ∀ a value. (a -> value) -> Decoder a -> Decoder value
tuple1 func d0 = do
    void $ arrayOfLengthT 1
    func <$> index 0 d0


-- | > Handle an array with exactly two elements. Useful for points and simple
-- | > pairs.
-- | >
-- | >     -- [3,4] or [0,0]
-- | >     point :: Decoder (Tuple Float Float)
-- | >     point =
-- | >         tuple2 Tuple float float
-- | >
-- | >     -- ["John","Doe"] or ["Hermann","Hesse"]
-- | >     name :: Decoder Name
-- | >     name =
-- | >         tuple2 Name string string
-- | >
-- | >     type Name = { first :: String, last :: String }
-- |
-- | This function was removed in Elm 0.18.
-- |
-- | Does not work with `equalDecoders` yet, but this is probably fixable.
tuple2 :: ∀ a b value. (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
tuple2 func d0 d1 = do
    void $ arrayOfLengthT 2

    v0 <- index 0 d0
    v1 <- index 1 d1

    pure $ func v0 v1


-- | > Handle an array with exactly three elements.
-- |
-- | This function was removed in Elm 0.18.
-- |
-- | Does not work with `equalDecoders` yet, but this is probably fixable.
tuple3 :: ∀ a b c value. (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
tuple3 func d0 d1 d2 = do
    void $ arrayOfLengthT 3

    v0 <- index 0 d0
    v1 <- index 1 d1
    v2 <- index 2 d2

    pure $ func v0 v1 v2


-- | This function was removed in Elm 0.18.
-- |
-- | Does not work with `equalDecoders` yet, but this is probably fixable.
tuple4 :: ∀ a b c d value. (a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder value
tuple4 func d0 d1 d2 d3 = do
    void $ arrayOfLengthT 4

    v0 <- index 0 d0
    v1 <- index 1 d1
    v2 <- index 2 d2
    v3 <- index 3 d3

    pure $ func v0 v1 v2 v3


-- | This function was removed in Elm 0.18.
-- |
-- | Does not work with `equalDecoders` yet, but this is probably fixable.
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
-- |
-- | Does not work with `equalDecoders` yet, but this is probably fixable.
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
-- |
-- | Does not work with `equalDecoders` yet, but this is probably fixable.
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
-- |
-- | Does not work with `equalDecoders` yet, but this is probably fixable.
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
