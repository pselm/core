[![Build Status](https://travis-ci.org/pselm/core.svg?branch=master)](https://travis-ci.org/pselm/core)

# Elm's `core` library in Purescript

This package is part of an effort aimed at people who know
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

## Compatibility

The modules are based on Elm 0.18, or version 5.1.1 of the Elm core libraries.
However, I have not necessarily been removing things that Elm removes, so some
things are documented as having been removed in Elm.

## The Modules

Here is a quick list of the modules in this package, and their Purescript analogues,
if any. In many cases, the implementation is a thin wrapper over the Purescript analog,
but some cases required more work, or a separate implementation. The links are to
the documentation for the module.

| Module             | Like          | From                 |
| ------------------ | ------------- | ---------------------|
| [Elm.Apply]        | Control.Apply | purescript-prelude   |
| [Elm.Array]        | Data.Sequence | purescript-sequences |
| [Elm.Basics]       | Prelude       | purescript-prelude   |
| [Elm.Bind]         | Control.Bind  | purescript-prelude   |
| [Elm.Bitwise]      | Data.Int.Bits | purescript-integers  |
| [Elm.Char]         | Data.Char     | purescript-strings   |
| [Elm.Color]        | Color         | purescript-colors    |
| [Elm.Date]         | Data.JSDate   | purescript-js-date   |
| [Elm.Debug]        | Debug.Trace   | purescript-debug     |
| [Elm.Default]      |               |                      |
| [Elm.Dict]         | Data.Map      | purescript-maps      |
| [Elm.Foldable]     | Data.Foldable | purescript-foldable-traversable |
| [Elm.FunctorWithIndex] | Data.FunctorWithIndex | purescript-foldable-traversable |
| [Elm.Json.Decode]  | Data.Foreign  | purescript-foreign   |
| [Elm.Json.Encode]  | Data.Foreign  | purescript-foreign   |
| [Elm.List]         | Data.List     | purescript-lists     |
| [Elm.Maybe]        | Data.Maybe    | purescript-maybe     |
| [Elm.Monoid]       | Data.Monoid   | purescript-monoid    |
| [Elm.Platform]     |               |                      |
| [Elm.Platform.Cmd] |               |                      |
| [Elm.Platform.Sub] |               |                      |
| [Elm.Process]      | Control.Monad.Aff  | purescript-aff        |                      |
| [Elm.Random]       | Test.QuickCheck.Arbitrary | purescript-quickcheck |
| [Elm.Regex]        | Data.String.Regex  | purescript-strings    |
| [Elm.Result]       | Data.Either        | purescript-either     |
| [Elm.Set]          | Data.Set           | purescript-sets       |
| [Elm.String]       | Data.String        | purescript-strings    |
| [Elm.Task]         | Conrol.Monad.IO    | purescript-io         |
| [Elm.Time]         | Data.Time.Duration | purescript-datetime   |
| [Elm.Trampoline]   | Control.Monad.Trampoline | purescript-free |
| [Elm.Transform2D]  | Graphics.Canvas    | purescript-canvas     |
| [Elm.Tuple]        | Data.Tuple         | purescript-tuples     |

[Elm.Apply]: generated-docs/Elm/Apply.md
[Elm.Array]: generated-docs/Elm/Array.md
[Elm.Basics]: generated-docs/Elm/Basics.md
[Elm.Bind]: generated-docs/Elm/Bind.md
[Elm.Bitwise]: generated-docs/Elm/Bitwise.md
[Elm.Char]: generated-docs/Elm/Char.md
[Elm.Color]: generated-docs/Elm/Color.md
[Elm.Date]: generated-docs/Elm/Date.md
[Elm.Debug]: generated-docs/Elm/Debug.md
[Elm.Default]: generated-docs/Elm/Default.md
[Elm.Dict]: generated-docs/Elm/Dict.md
[Elm.Foldable]: generated-docs/Elm/Foldable.md
[Elm.FunctorWithIndex]: generated-docs/Elm/FunctorWithIndex.md
[Elm.Json.Decode]: generated-docs/Elm/Json/Decode.md
[Elm.Json.Encode]: generated-docs/Elm/Json/Encode.md
[Elm.List]: generated-docs/Elm/List.md
[Elm.Maybe]: generated-docs/Elm/Maybe.md
[Elm.Monoid]: generated-docs/Elm/Monoid.md
[Elm.Platform]: generated-docs/Elm/Platform.md
[Elm.Platform.Cmd]: generated-docs/Elm/Platform/Cmd.md
[Elm.Platform.Sub]: generated-docs/Elm/Platform/Sub.md
[Elm.Process]: generated-docs/Elm/Process.md
[Elm.Random]: generated-docs/Elm/Random.md
[Elm.Regex]: generated-docs/Elm/Regex.md
[Elm.Result]: generated-docs/Elm/Result.md
[Elm.Set]: generated-docs/Elm/Set.md
[Elm.String]: generated-docs/Elm/String.md
[Elm.Task]: generated-docs/Elm/Task.md
[Elm.Time]: generated-docs/Elm/Time.md
[Elm.Trampoline]: generated-docs/Elm/Trampoline.md
[Elm.Transform2D]: generated-docs/Elm/Transform2D.md
[Elm.Tuple]: generated-docs/Elm/Tuple.md

## Installation

Try `bower install pselm/core`

## Development

Try something like:

    git clone https://github.com/pselm/core
    npm install
    npm test

## API

See the table above for links to the documentation for each module.
