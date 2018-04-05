/* global exports */
"use strict";

// module Elm.Random

// Elm's Random does an Elm.Basics mod, but on a value that is a Javascript
// float. So, it's difficult to represent properly in Purescript ... better to
// special-case it here.
exports.floatMod = function floatMod (a) {
    return function (b) {
        // The following code is derived from the Elm implementation here:
        //
        // https://github.com/elm-lang/core/blob/568b384720995ce35b9561fab89f2c0b63c2c3fc/src/Native/Basics.js#L13-L23
        if (b === 0)
        {
            throw new Error('Cannot perform mod 0. Division by zero error.');
        }
        var r = a % b;
        var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r + b) : -floatMod(-a)(-b));

        return m === b ? 0 : m;
    }
};
