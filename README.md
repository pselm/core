[![Latest release](http://img.shields.io/bower/v/purescript-elm-compat.svg)](https://github.com/rgrempel/purescript-elm-compat/releases)
[![Dependency Status](https://www.versioneye.com/user/projects/5701e80bfcd19a00415afff5/badge.svg?style=flat)](https://www.versioneye.com/user/projects/5701e80bfcd19a00415afff5)
[![Build Status](https://travis-ci.org/rgrempel/purescript-elm-compat.svg?branch=master)](https://travis-ci.org/rgrempel/purescript-elm-compat)

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
in progress, will deal with the Elm architecture (e.g. commands, subscriptions, etc.).
However, I thought that this package might already be of some help
to someone.

## Compatibility

The modules are based on Elm 0.18, or version 5.1.1 of the Elm core libraries.
However, I have not necessarily been removing things that Elm removes, so some
things are documented as having been removed in Elm.

## The Modules

Here is a quick list of the modules in this package, and their Purescript analogues,
if any. In many cases, the implementation is a thin wrapper over the Purescript analog,
but some cases required more work, or a separate implementation.

| Module          | Like          | From                 |
| --------------- | ------------- | ---------------------|
| Elm.Apply       | Control.Apply | purescript-prelude   |
| Elm.Array       | Data.Sequence | purescript-sequences |
| Elm.Basics      | Prelude       | purescript-prelude   |
| Elm.Bind        | Control.Bind  | purescript-prelude   |
| Elm.Bitwise     | Data.Int.Bits | purescript-integers  |
| Elm.Char        | Data.Char     | purescript-strings   |
| Elm.Color       | Color         | purescript-colors    |
| Elm.Date        | Data.JSDate   | purescript-js-date   |
| Elm.Debug       | Debug.Trace   | purescript-debug     |
| Elm.Dict        | Data.Map      | purescript-maps      |
| Elm.Foldable    | Data.Foldable | purescript-foldable-traversable |
| Elm.Json.Decode | Data.Foreign  | purescript-foreign   |
| Elm.Json.Encode | Data.Foreign  | purescript-foreign   |
| Elm.List        | Data.List     | purescript-lists     |
| Elm.Maybe       | Data.Maybe    | purescript-maybe     |
| Elm.Platform    |               |                      |
| Elm.Platform.Cmd |              |                      |
| Elm.Platform.Sub |              |                      |
| Elm.Process     |               |                      |
| Elm.Random      | Test.QuickCheck.Arbitrary | purescript-quickcheck |
| Elm.Regex       | Data.String.Regex  | purescript-strings  |
| Elm.Result      | Data.Either        | purescript-either   |
| Elm.Set         | Data.Set           | purescript-sets     |
| Elm.String      | Data.String        | purescript-strings  |
| Elm.Task        | Conrol.Monad.Aff   | purescript-aff      |
| Elm.Time        | Data.Time.Duration | purescript-datetime |
| Elm.Trampoline  | Control.Monad.Trampoline | purescript-free |
| Elm.Transform2D | Graphics.Canvas | purescript-canvas |
| Elm.Tuple       | Data.Tuple      | purescript-tuples |

## Installation

Try `bower install purescript-elm-compat`

## Development

Try something like:

    git clone https://github.com/rgrempel/purescript-elm-compat
    npm install
    npm test

## API

Documentation for the API can be found on
[Pursuit](https://pursuit.purescript.org/packages/purescript-elm-compat).
Or, if you are already looking at Pursuit, then below ...

