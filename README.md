
# purescript-elm-compat

This package is the first fruits of an effort aimed at people who know
[Elm](http://elm-lang.org) well and wish to give
[Purescript](http://purescript.org) a try. The idea is to make it as easy
as possible to take Elm code (and Elm knowledge) and use it in Purescript.

The modules in this package are Purescript equivalents of Elm core modules,
with `Elm.` tacked on to the beginning. So, Elm's `Maybe` becomes
`Elm.Maybe`, Elm's `List` becomes `Elm.List`, etc.

With a few exceptions, the implementation wraps some existing
Purescript module, making whatever adjustments are necessary to maintain
the Elm API as closely as possible. Thus, this package is unlikely to
be of interest to people who do not know Elm -- there is already a
more direct way to do everything this package does.

The [larger project](https://github.com/rgrempel/purescript-elm), still
in progress, will also deal with things such as tasks, signals, graphics, HTML,
etc. However, I thought that this package might already be of some help
to someone.

## Compatibility

The modules are based on Elm 0.16, or version 3.0 of the Elm core libraries.

## The Modules

Here is a quick list of the modules in this package, and their Purescript analogues.
The full API is available below.

* `Elm.Apply` -- Like Purescript's `Apply`, but with different function names
* `Elm.Array` -- Implemented via `Data.Sequence`
* `Elm.Basics` -- Like Purescript's `Prelude`
* `Elm.Bind` -- Like Purescript's `Bind`, but uses `andThen` for `bind`
* `Elm.Bitwise` -- Implemented via `Data.Int.Bits`
* `Elm.Char` -- Implemented via `Data.Char`
* `Elm.Date` -- Implemented via `Data.Date`
* `Elm.Debug` -- Implemented via `Debug.Trace` and `Partial.Unsafe`
* `Elm.Dict` -- Implemented via `Data.Map`
* `Elm.Foldable` -- Like Purescript's `Foldable`, but with a different signature for `foldl`
* `Elm.Json.Decode` -- Implemented using `Data.Foreign`, with a fair bit of sugar applied.
* `Elm.Json.Encode` -- Implemented using `Data.Foreign`
* `Elm.List` -- Implemented via `Data.List`
* `Elm.Maybe` -- Implemented via `Data.Maybe`
* `Elm.Random` -- A translation from Elm's implementation
* `Elm.Regex` -- Similar to `Data.String.Regex`, but re-implemented due to irreconcilable API differences.
* `Elm.Result` -- Like Purescript's `Data.Either`, but re-implemented to preserve the Elm constructor names (`Ok` and `Err` rather than `Right` and `Left`).
* `Elm.Set` -- Implemented via `Data.Set`
* `Elm.String` -- Implemented via `Data.String`
* `Elm.Trampoline` -- A translation from Elm's implementation.

## Installation

Try `bower install purescript-elm-compat`

## Development

Try something like:

    git clone https://github.com/rgrempel/purescript-elm-compat
    bower install
    pulp test

## API

Documentation for the API can be found on
[Pursuit](https://pursuit.purescript.org/packages/purescript-elm-compat).
Or, if you are already looking at Pursuit, then below ...

