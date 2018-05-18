## Module Elm.Json.Decode

> Turn JSON values into Elm values. Definitely check out this [intro to
> JSON decoders][guide] to get a feel for how this library works!

[guide]: https://guide.elm-lang.org/interop/json.html

Elm's `Json.Decode` doesn't seem to be quite like any existing Purescript
package, so I've re-implemented it, using parts of purescript-foreign as
a base. For other approaches to decoding JSON in Purescript, you could see
purescript-foreign, and the purescript-argonaut packages. It would
probasbly be a good idea to extend this code by allowing for integration
with purescript-argonaut as an option in addition to purescript-foreign.

The key difference between Elm's approach to decoders and what
purescript-foreign or purescript-argonaut do is that Elm needs to be
able to make some kind of decision about the equality of two decoders, in
order for the virtual DOM to decide whether a listener can be kept or
needs to be removed and re-added.  This drives the design of this module
towards a kind of DSL that allows `equalDecoders` to do a little bit more
than just compare decoders for referential equality, at least in some cases.
I've documented how the various functions in this module interact with
`equalDecoders`.

Note that (so far) we're not trying to preserve all the nice error messages
that Elm gives ... we could do a better job of that. (Elm uses a different
approach to JSON errors in Elm 0.19, so I may well wait for that in order
to do something nicer with errors here).

#### `Decoder`

``` purescript
data Decoder a
```

> A value that knows how to decode JSON values.

Here are some notes about the degree to which the instances preserve the
ability of `equalDecoders` to detect equality. See the docs for
`equalDecoders` for a more general explanation -- it can always detect the
equality of decoders that are the very same decoder (i.e. with the same
reference), so the question is how well it can deteect the equality of
decoders that are separately constructed (not the very same reference).

The `Functor` instance preserves equality detection so long as the
function you supply to `map` is referentially equal in each case (since we
can't check functions for equality except via referential equality). So,
it is better to `map` with a function that you've pulled out to the
top-level, with a stable reference, rather than a function defined inline
as a lambda. Of course, the decoder you supply to `map` must also itself
have preserved equality detection.

The `Alt` instance preserves equality detection. Of course, the decoders
you supply to `alt` or `<|>` must themselves have been constructed in a
way that preserves equality detection.

The `Plus` instance preserves equality detection. (I.e. one `empty` decoder
will be equal to another, and should interact with `<|>` and `oneOf` in
the correct ways).

The `Apply` instance preserves equality detection. Thus, the results of
(to the extent that the inputs did).

The `Applicative` instance isn't able to insist on an `Eq` constraint for
the value you supply to `pure`. Thus, `equalDecoders` will be limited to
using referential equality on values you supply to `pure`. So, it will be
preferable to use `succeed` directly where you can.

The `Bind` instance preserves equality detection if the supplied function
is referentially equal in each case. This is a little awkward for
do-notation, or an `|> andThen` pipeline (the roughly-equivalent Elm
idiom), since in those cases the functions will typically be defined
inline, and thus not with a stable reference. However, if you can stick to
functions with stable references, rather than defined inline, (>>=) and
`andThen` will preserve equality.

##### Instances
``` purescript
Functor Decoder
Alt Decoder
Plus Decoder
Apply Decoder
Applicative Decoder
Bind Decoder
Monad Decoder
```

#### `decodeString`

``` purescript
decodeString :: forall a. Decoder a -> String -> Result String a
```

> Parse the given string into a JSON value and then run the `Decoder` on it.
> This will fail if the string is not well-formed JSON or if the `Decoder`
> fails for some reason.

>     decodeString int "4"     == Ok 4
>     decodeString int "1 + 2" == Err ...

#### `decodeValue`

``` purescript
decodeValue :: forall a. Decoder a -> Value -> Result String a
```

> Run a `Decoder` on some JSON `Value`. You can send these JSON values
> through ports, so that is probably the main time you would use this function.

#### `fromForeign`

``` purescript
fromForeign :: forall a. (Foreign -> F a) -> Decoder a
```

Given a function which reads a `Foreign`, make a decoder.

Note that this is not in the Elm API.

Because you are supplying a function, `equalDecoders` will only consider
the resulting decoders equal if the function you supply is referentially
equal to the function you supply in the other case.  So, to preserve
equality, the supplied function should not be a lambda -- it should be a
top-level function definition. See the docs for `equalDecoders` for more
discussion.

#### `string`

``` purescript
string :: Decoder String
```

> Decode a JSON string into an Elm `String`.
>
>     decodeString string "true"              == Err ...
>     decodeString string "42"                == Err ...
>     decodeString string "3.14"              == Err ...
>     decodeString string "\"hello\""         == Ok "hello"
>     decodeString string "{ \"hello\": 42 }" == Err ...

Works with `equalDecoders`

#### `int`

``` purescript
int :: Decoder Int
```

> Decode a JSON number into an Elm `Int`.
>
>     decodeString int "true"              == Err ...
>     decodeString int "42"                == Ok 42
>     decodeString int "3.14"              == Err ...
>     decodeString int "\"hello\""         == Err ...
>     decodeString int "{ \"hello\": 42 }" == Err ...

Works with `equalDecoders`

#### `float`

``` purescript
float :: Decoder Float
```

> Decode a JSON number into an Elm `Float`.
>
>     decodeString float "true"              == Err ..
>     decodeString float "42"                == Ok 42
>     decodeString float "3.14"              == Ok 3.14
>     decodeString float "\"hello\""         == Err ...
>     decodeString float "{ \"hello\": 42 }" == Err ...

Works with `equalDecoders`

#### `bool`

``` purescript
bool :: Decoder Bool
```

> Decode a JSON boolean into an Elm `Bool`.
>
>     decodeString bool "true"              == Ok True
>     decodeString bool "42"                == Err ...
>     decodeString bool "3.14"              == Err ...
>     decodeString bool "\"hello\""         == Err ...
>     decodeString bool "{ \"hello\": 42 }" == Err ...

Works with `equalDecoders`

#### `null`

``` purescript
null :: forall a. Eq a => a -> Decoder a
```

> Decode a `null` value into some Elm value.
>
>     decodeString (null False) "null" == Ok False
>     decodeString (null 42) "null"    == Ok 42
>     decodeString (null 42) "42"      == Err ..
>     decodeString (null 42) "false"   == Err ..
>
> So if you ever see a `null`, this will return whatever value you specified.

Works with `equalDecoders`

#### `null_`

``` purescript
null_ :: forall a. a -> Decoder a
```

Like `null`, but for cases where your default value does not have an `Eq`
instance. Use `null` where you can, because it will make `equalDecoders`
more reliable.

#### `list`

``` purescript
list :: forall a. Decoder a -> Decoder (List a)
```

> Decode a JSON array into an Elm `List`.
>
>     decodeString (list int) "[1,2,3]"       == Ok [1,2,3]
>     decodeString (list bool) "[true,false]" == Ok [True,False]

Preserves equality-checking for the input with `equalDecoders`

You can also use `unfoldable` to decode into any container type that has
an `Unfoldable` instance.

#### `array`

``` purescript
array :: forall a. Decoder a -> Decoder (Array a)
```

> Decode a JSON array into an Elm `Array`.
>
>     decodeString (array int) "[1,2,3]"       == Ok (Array.fromList [1,2,3])
>     decodeString (array bool) "[true,false]" == Ok (Array.fromList [True,False])

Preserves equality-checking for the input with `equalDecoders`

You can also use `unfoldable` to decode into any container type that has
an `Unfoldable` instance.

#### `unfoldable`

``` purescript
unfoldable :: forall f a. Unfoldable f => Decoder a -> Decoder (f a)
```

Extract any `Unfoldable` from a JS array.

    -- [1,2,3,4]

    numbers :: Decoder (Array Int)
    numbers =
        unfoldable int

Note that this is not part of the Elm API.

Preserves equality-checking for the input with `equalDecoders`

#### `tuple1`

``` purescript
tuple1 :: forall a value. (a -> value) -> Decoder a -> Decoder value
```

> Handle an array with exactly one element.
>
>     extractString :: Decoder String
>     extractString =
>         tuple1 identity string
>
>     authorship :: Decoder String
>     authorship =
>         oneOf
>           [ tuple1 (\author -> "Author: " <> author) string
>           , list string |> map (\authors -> "Co-authors: " <> String.join ", " authors)
>           ]

This function was removed in Elm 0.18.

Does not work with `equalDecoders` yet, but this is probably fixable.

#### `tuple2`

``` purescript
tuple2 :: forall a b value. (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
```

> Handle an array with exactly two elements. Useful for points and simple
> pairs.
>
>     -- [3,4] or [0,0]
>     point :: Decoder (Tuple Float Float)
>     point =
>         tuple2 Tuple float float
>
>     -- ["John","Doe"] or ["Hermann","Hesse"]
>     name :: Decoder Name
>     name =
>         tuple2 Name string string
>
>     type Name = { first :: String, last :: String }

This function was removed in Elm 0.18.

Does not work with `equalDecoders` yet, but this is probably fixable.

#### `tuple3`

``` purescript
tuple3 :: forall a b c value. (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
```

> Handle an array with exactly three elements.

This function was removed in Elm 0.18.

Does not work with `equalDecoders` yet, but this is probably fixable.

#### `tuple4`

``` purescript
tuple4 :: forall a b c d value. (a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder value
```

This function was removed in Elm 0.18.

Does not work with `equalDecoders` yet, but this is probably fixable.

#### `tuple5`

``` purescript
tuple5 :: forall a b c d e value. (a -> b -> c -> d -> e -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder value
```

This function was removed in Elm 0.18.

Does not work with `equalDecoders` yet, but this is probably fixable.

#### `tuple6`

``` purescript
tuple6 :: forall a b c d e f value. (a -> b -> c -> d -> e -> f -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder value
```

This function was removed in Elm 0.18.

Does not work with `equalDecoders` yet, but this is probably fixable.

#### `tuple7`

``` purescript
tuple7 :: forall a b c d e f g value. (a -> b -> c -> d -> e -> f -> g -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder value
```

This function was removed in Elm 0.18.

Does not work with `equalDecoders` yet, but this is probably fixable.

#### `tuple8`

``` purescript
tuple8 :: forall a b c d e f g h value. (a -> b -> c -> d -> e -> f -> g -> h -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder value
```

This function was removed in Elm 0.18.

Does not work with `equalDecoders` yet, but this is probably fixable.

#### `field`

``` purescript
field :: forall a. String -> Decoder a -> Decoder a
```

> Decode a JSON object, requiring a particular field.
>
>     decodeString (field "x" int) "{ \"x\": 3 }"            == Ok 3
>     decodeString (field "x" int) "{ \"x\": 3, \"y\": 4 }"  == Ok 3
>     decodeString (field "x" int) "{ \"x\": true }"         == Err ...
>     decodeString (field "x" int) "{ \"y\": 4 }"            == Err ...
>
>     decodeString (field "name" string) "{ \"name\": \"tom\" }" == Ok "tom"
>
> The object *can* have other fields. Lots of them! The only thing this decoder
> cares about is if `x` is present and that the value there is an `Int`.
>
> Check out [`map2`](#map2) to see how to decode multiple fields!

`equalDecoders` will consider the resulting decoder equal to another
produced by this function if the field names are equal and the supplied
decoders are themselves considered equal by `equalDecoders`. So, it is
equality-preserving.

#### `(:=)`

``` purescript
infixl 4 field as :=
```

#### `at`

``` purescript
at :: forall f a. Foldable f => f String -> Decoder a -> Decoder a
```

> Decode a nested JSON object, requiring certain fields.
>
>     json = """{ "person": { "name": "tom", "age": 42 } }"""
>
>     decodeString (at ["person", "name"] string) json  == Ok "tom"
>     decodeString (at ["person", "age" ] int   ) json  == Ok "42
>
> This is really just a shorthand for saying things like:
>
>     field "person" (field "name" string) == at ["person","name"] string

Note that the signature is defined in terms of `Foldable` so that it will
work with `Array` or `List` (among others).

Preserves equality for `equalDecoders`. The resulting decoder will also be
considered equal with decoders constructed manually with nested `field`
applications, if the field names match.

#### `index`

``` purescript
index :: forall a. Int -> Decoder a -> Decoder a
```

> Decode a JSON array, requiring a particular index.
>
>     json = """[ "alice", "bob", "chuck" ]"""
>
>     decodeString (index 0 string) json  == Ok "alice"
>     decodeString (index 1 string) json  == Ok "bob"
>     decodeString (index 2 string) json  == Ok "chuck"
>     decodeString (index 3 string) json  == Err ...

This function was added in Elm 0.18.

`equalDecoders` will consider the resulting decoder equal to another
produced by this function if the indexes are equal and the supplied
decoders are themselves considered equal by `equalDecoders`. So, it is
equality-preserving.

#### `object1`

``` purescript
object1 :: forall a value. (a -> value) -> Decoder a -> Decoder value
```

> Apply a function to a decoder.
>
>     object1 sqrt ("x" := float)

Equivalent to Purescript's `map`.

Removed in Elm 0.18, in favour of `map`.

Works with `equalDecoders` so long as the function supplied in one case is
referentially equal to the function supplied in the other.

#### `object2`

``` purescript
object2 :: forall a b value. (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
```

> Use two different decoders on a JS value. This is nice for extracting
> multiple fields from an object.
>
>     point :: Decoder (Tuple Float Float)
>     point =
>         object2 Tuple
>           ("x" := float)
>           ("y" := float)

Equivalent to Purescript's `lift2`.

Removed in Elm 0.18, in favour of `map2`.

Works with `equalDecoders` so long as the function supplied in one case is
referntially equal to the function supplied in the other case, and the
provided decoders preserve equality.

#### `object3`

``` purescript
object3 :: forall a b c value. (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
```

> Use three different decoders on a JS value. This is nice for extracting
> multiple fields from an object.
>
>     type Job = { name :: String, id :: Int, completed :: Bool }
>
>     job :: Decoder Job
>     job =
>         object3 Job
>           ("name" := string)
>           ("id" := int)
>           ("completed" := bool)

Equivalent to Purescript's `lift3`.

Removed in Elm 0.18, in favour of `map3`.

Works with `equalDecoders` so long as the function supplied in one case is
referntially equal to the function supplied in the other case, and the
provided decoders preserve equality.

#### `object4`

``` purescript
object4 :: forall a b c d value. (a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder value
```

Equivalent to Purescript's `lift4`.

Removed in Elm 0.18, in favour of `map4`.

Works with `equalDecoders` so long as the function supplied in one case is
referntially equal to the function supplied in the other case, and the
provided decoders preserve equality.

#### `object5`

``` purescript
object5 :: forall a b c d e value. (a -> b -> c -> d -> e -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder value
```

Equivalent to Purescript's `lift5`.

Removed in Elm 0.18, in favour of `map5`.

Works with `equalDecoders` so long as the function supplied in one case is
referntially equal to the function supplied in the other case, and the
provided decoders preserve equality.

#### `object6`

``` purescript
object6 :: forall a b c d e f value. (a -> b -> c -> d -> e -> f -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder value
```

Removed in Elm 0.18, in favour of `map6`.

Works with `equalDecoders` so long as the function supplied in one case is
referntially equal to the function supplied in the other case, and the
provided decoders preserve equality.

#### `object7`

``` purescript
object7 :: forall a b c d e f g value. (a -> b -> c -> d -> e -> f -> g -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder value
```

Removed in Elm 0.18, in favour of `map7`.

Works with `equalDecoders` so long as the function supplied in one case is
referntially equal to the function supplied in the other case, and the
provided decoders preserve equality.

#### `object8`

``` purescript
object8 :: forall a b c d e f g h value. (a -> b -> c -> d -> e -> f -> g -> h -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder value
```

Removed in Elm 0.18, in favour of `map8`.

Works with `equalDecoders` so long as the function supplied in one case is
referntially equal to the function supplied in the other case, and the
provided decoders preserve equality.

#### `keyValuePairs`

``` purescript
keyValuePairs :: forall f a. Monoid (f (Tuple String a)) => Applicative f => Decoder a -> Decoder (f (Tuple String a))
```

> Decode a JSON object into an Elm `List` of pairs.
>
>     decodeString (keyValuePairs int) "{ \"alice\": 42, \"bob\": 99 }"
>       == [("alice", 42), ("bob", 99)]

The container for the return type is polymorphic in order to accommodate `List` or `Array`, among others.

Does not work with `equalDecoders` yet, but this should be fixable.

#### `dict`

``` purescript
dict :: forall a. Decoder a -> Decoder (Dict String a)
```

> Decode a JSON object into an Elm `Dict`.
>
>     decodeString (dict int) "{ \"alice\": 42, \"bob\": 99 }"
>       == Dict.fromList [("alice", 42), ("bob", 99)]

Does not work with `equalDecoders` yet, but should be fixable.

#### `nullable`

``` purescript
nullable :: forall a. Decoder a -> Decoder (Maybe a)
```

> Decode a nullable JSON value into an Elm value.
>
>     decodeString (nullable int) "13"    == Ok (Just 13)
>     decodeString (nullable int) "42"    == Ok (Just 42)
>     decodeString (nullable int) "null"  == Ok Nothing
>     decodeString (nullable int) "true"  == Err ..

This function was added in Elm 0.18.

For `equalDecoders`, this preserves whatever answer would be given for the
inputs.

#### `maybe`

``` purescript
maybe :: forall a. Decoder a -> Decoder (Maybe a)
```

> Helpful for dealing with optional fields. Here are a few slightly different
> examples:
>
>     json = """{ "name": "tom", "age": 42 }"""
>
>     decodeString (maybe (field "age"    int  )) json == Ok (Just 42)
>     decodeString (maybe (field "name"   int  )) json == Ok Nothing
>     decodeString (maybe (field "height" float)) json == Ok Nothing
>
>     decodeString (field "age"    (maybe int  )) json == Ok (Just 42)
>     decodeString (field "name"   (maybe int  )) json == Ok Nothing
>     decodeString (field "height" (maybe float)) json == Err ...
>
> Notice the last example! It is saying we *must* have a field named `height` and
> the content *may* be a float. There is no `height` field, so the decoder fails.
>
> Point is, `maybe` will make exactly what it contains conditional. For optional
> fields, this means you probably want it *outside* a use of `field` or `at`.

`equalDecoders` will consider the results of this function to be equal if
the provided decoder is equal ... that is, `maybe` is equal-preserving.

#### `fail`

``` purescript
fail :: forall a. String -> Decoder a
```

> Ignore the JSON and make the decoder fail. This is handy when used with
> `oneOf` or `andThen` where you want to give a custom error message in some
> case.

`equalDecoders` considers two `fail` decoders to be equal if they have the
same message.

#### `succeed`

``` purescript
succeed :: forall a. Eq a => a -> Decoder a
```

> Ignore the JSON and produce a certain Elm value.
>
>     decodeString (succeed 42) "true"    == Ok 42
>     decodeString (succeed 42) "[1,2,3]" == Ok 42
>     decodeString (succeed 42) "hello"   == Err ... -- this is not a valid JSON string
>
> This is handy when used with `oneOf` or `andThen`.

Works well with `equalDecoders`.

#### `succeed_`

``` purescript
succeed_ :: forall a. a -> Decoder a
```

Like `succeed`, but for cases where your value does not have an
`Eq` instance. Using `succeed` instead will make `equalDecoders`
more reliable -- without an `Eq` instance, we have to rely on
referential equality.

#### `value`

``` purescript
value :: Decoder Value
```

> Do not do anything with a JSON value, just bring it into Elm as a `Value`.
> This can be useful if you have particularly crazy data that you would like to
> deal with later. Or if you are going to send it out a port and do not care
> about its structure.

Works with `equalDecoders`

#### `customDecoder`

``` purescript
customDecoder :: forall a b. Decoder a -> (a -> Result String b) -> Decoder b
```

> Create a custom decoder that may do some fancy computation.

This function was removed in Elm 0.18.

`equalDecoders` will consider the resulting decoders equal if the input
decoders are equal, and the provided functions in each case are
referentially equal.

#### `lazy`

``` purescript
lazy :: forall a. (Unit -> Decoder a) -> Decoder a
```

> Sometimes you have JSON with recursive structure, like nested comments.
> You can use `lazy` to make sure your decoder unrolls lazily.
>
>     type alias Comment =
>       { message : String
>       , responses : Responses
>       }
>
>     type Responses = Responses (List Comment)
>
>     comment : Decoder Comment
>     comment =
>       map2 Comment
>         (field "message" string)
>         (field "responses" (map Responses (list (lazy (\_ -> comment)))))
>
> If we had said `list comment` instead, we would start expanding the value
> infinitely. What is a `comment`? It is a decoder for objects where the
> `responses` field contains comments. What is a `comment` though? Etc.
>
> By using `list (lazy (\_ -> comment))` we make sure the decoder only expands
> to be as deep as the JSON we are given. You can read more about recursive data
> structures [here][].
>
> [here]: https://github.com/elm-lang/elm-compiler/blob/master/hints/recursive-alias.md

This function was added in Elm 0.18.

This function works with `equalDecoders` so long as you provide functions
that are referentially equal ... see the docs for `equalDecoders` for more
information.  That's probably the best we can do, since the point of
`lazy` is to avoid unrolling the actual decoder until needed.

#### `equalDecoders`

``` purescript
equalDecoders :: forall a. Decoder a -> Decoder a -> Bool
```

`equalDecoders` attempts to compare two decoders for equality. It is
subject to false negatives, but positives should be reliable. (For this
reason, we don't provide an `Eq` instance for `Decoder` ... this is a
function that has a specialized use, rather than being a fully reliable
test for equality).

This is roughly equivalent to a function that Elm uses internally (not
exposed in Elm) as part of the virtual DOM, to decide whether a listener
must be removed and re-applied (because it uses a different decoder than
the previously applied listener). So, false negatives are an efficiency
issue (as listeners will be removed and re-applied unnecessarily), while
false positives would be a more serious problem (and should not occur).

I have documented, for each function in the module, how well it preserves
the ability of `equalDecoders` to detect equality. The cases fall roughly
into these categories. (Elm's behaviour with respect to detecting equality
is roughly similar, I believe).

### The very same decoder

If two decoders are the very same thing (i.e. referentially equal), then
`equalDecoders` will reliably detect that. So, if your `view` code
references a decoder by its top=level name, then `equalDecoders` will
detect that the decoder is equal to itself, on the next round. The decoder
may have been constructed in whatever complex way is necessary, but if you
refer to it via its top-level name (not a function call), then
`equalDecoders` will work well with it. So, in cases where it is possible
to define your decoder at the top-level, that is handy.

So, if you define a decoder like this:

    decodePerson :: Decoder Person
    decodePerson =
       ...

... that is, as a value, without arguments, then it doesn't matter what
you do in the `...` to construct the decoder ... `equalDecoders` will be
able to detect that `decodePerson` is equal to `decodePerson`.

For a decoder to take advantage of this, it must be defined without taking
arguments, and must be defined at the top-level (i.e. not inside a `let`
expression). Otherwise, the decoder won't have a stable reference. It may
still compare successfully with `equalDecoders`, but that will depend on
exactly how it is constructed. If the decoder has a stable reference, then
it doesn't matter how it was constructed.

### Not the very same decoder

If two decoders are not the very same thing, then whether `equalDecoders`
can successfully detect equality depends on how they were constructed and
combined. I've documented the effect of each function in this module on
the detection of equality, but the general rules are as follows:

- The equality of primitive decoders can always be detected

  e.g. `float`, `int`, `bool`, `string`, `value`

- If you have to supply an argument that is a function, then we can only
  detect equality if you supply a function that is referentially equal in
  each case. So, it's better to avoid defining functions "inline" as a
  lambda when creating a decoder. Instead, try to pull the functions out
  to the top-level where you can, so they will have stable references for
  the purpose of testing referential equality.

  e.g. `map`, `andThen`, `bind`, `fromForeign`, `customDecoder`, `lazy`

  Note that for `bind`, this means that we'll have a limited ability to
  detect the equality of decoders defined using `do` notation (or an `|>
  andThen` pipeline, in the equivalent Elm idiom). Since the repeated
  "binds" are defined inline, they won't have stable references. However,
  if you can pull all but the first `bind` out into a stable reference,
  then a single `>>=` (or `andThen`) which refers to the stable reference
  will preserve equality.

- If you have to supply other decoders as arguments, then generally we
  preserve equality detection. That is, the resulting decoder will generally
  work as well with `equalDecoders` as the decoders you supply.

  e.g. `alt`, `<|>`, `oneOf`, `field`, `at`, `index`, `field`, `list`, `array`,
       `unfoldable`, `nullable`, `maybe`

- If you supply values as an argument, then `equalDecoders` works best if
  you use functions that require an `Eq` instance. In those cases, we can
  use the `Eq` instance to compare the values when detecting the equality
  of decoders. Otherwise, we have to fall back on referential equality.
  So, prefer `succeed` and `null` to `succeed_` and `null_`.

  One way in which this is a little awkward is that a "bare" record type
  cannot have an `Eq` instance -- you will need to make a `newtype` for
  it. However, once you've done that, the compiler can often derive an
  `Eq` instance for you (along with providing various other
  newtype-related conveniences), so it is only a mild nuisance. (Elm
  instead has a magic `==` that works with bare record types, though not
  without its own difficulties -- there is no free lunch here).

- There are some functions which currently destroy the ability to detect
  equality (unless you keep a stable reference to the result), but which
  should be fixable.

  e.g. `keyValuePairs`, `dict`, and `tuple1` through `tuple8`

#### `equalDecoders_`

``` purescript
equalDecoders_ :: forall a b. Decoder a -> Decoder b -> Bool
```

Like `equalDecoders`, but doesn't rely on the decoders being of the same
type. If you know the decoders are of the same type, `equalDecoders` can
do a somewhat better job of determining equality.

#### `equalDecodersL`

``` purescript
equalDecodersL :: forall a b. Maybe (a ~ b) -> Decoder a -> Decoder b -> Bool
```

We can do slightly different things depending on whethr we'd got evidence
that the two decoders are of the same type. So, the first parameter
indicates whether we've got that evidence or not.


### Re-exported from Data.Foldable:

#### `oneOf`

``` purescript
oneOf :: forall f g a. Foldable f => Plus g => f (g a) -> g a
```

Combines a collection of elements using the `Alt` operation.

### Re-exported from Elm.Apply:

#### `map8`

``` purescript
map8 :: forall w a b c d e f g h i. Apply w => (a -> b -> c -> d -> e -> f -> g -> h -> i) -> w a -> w b -> w c -> w d -> w e -> w f -> w g -> w h -> w i
```

Map a function of eight arguments over some container type.

#### `map7`

``` purescript
map7 :: forall w a b c d e f g h. Apply w => (a -> b -> c -> d -> e -> f -> g -> h) -> w a -> w b -> w c -> w d -> w e -> w f -> w g -> w h
```

Map a function of seven arguments over some container type.

#### `map6`

``` purescript
map6 :: forall w a b c d e f g. Apply w => (a -> b -> c -> d -> e -> f -> g) -> w a -> w b -> w c -> w d -> w e -> w f -> w g
```

Map a function of six arguments over some container type.

#### `map5`

``` purescript
map5 :: forall w a b c d e f. Apply w => (a -> b -> c -> d -> e -> f) -> w a -> w b -> w c -> w d -> w e -> w f
```

Map a function of five arguments over some container type.

The equivalent of Purescript's `lift5`.

#### `map4`

``` purescript
map4 :: forall w a b c d e. Apply w => (a -> b -> c -> d -> e) -> w a -> w b -> w c -> w d -> w e
```

Map a function of four arguments over some container type.

The equivalent of Purescript's `lift4`.

#### `map3`

``` purescript
map3 :: forall w a b c d. Apply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
```

Map a function of three arguments over some container type.

The equivalent of Purescript's `lift3`.

#### `map2`

``` purescript
map2 :: forall w a b c. Apply w => (a -> b -> c) -> w a -> w b -> w c
```

Map a function of two arguments over some container type.

The equivalent of Purescript's `lift2`.

#### `andMap`

``` purescript
andMap :: forall a b f. Apply f => f (a -> b) -> f a -> f b
```

Map a function in a container to a value in a container.

This is the equivalent of Purescript's `apply`.

### Re-exported from Elm.Bind:

#### `andThen`

``` purescript
andThen :: forall m a b. Bind m => (a -> m b) -> m a -> m b
```

Given some computation, chain its result with another computation.

Equivalent to Purescript's `bind`.

The order of the arguments was flipped in Elm 0.18.

### Re-exported from Elm.Json.Encode:

#### `Value`

``` purescript
type Value = Foreign
```

Represents a JavaScript value.

### Re-exported from Prelude:

#### `map`

``` purescript
map :: forall a b f. Functor f => (a -> b) -> f a -> f b
```

