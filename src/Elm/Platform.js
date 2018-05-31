/* global exports */
"use strict";

// module Elm.Platform

exports.exportProgramImpl = function (namespace) {
    return function (worker) {
        return function () {
            // This sets up an `Elm` export in the way that code inserted
            // by the Elm compiler would do it.
            var Elm = {};
            Elm[namespace] = {};

            // The Elm code actually supplies `Elm[namespace]` to the program
            // for it to fill in `worker`, `embed`, and/or `fullscreen` ...  I
            // suppose I could do that as well?
            Elm[namespace].worker = function (flags) {
                worker(flags)();
            };

            // Elm's compiler does some setup for AMD and module definitions if
            // it finds them ... I suppose we could do something similar? For
            // now, I'll just assume a global `Elm`.
            var globalCtx = typeof window != 'undefined' ? window : (typeof global != 'undefined' ? global : self);
            var globalElm = globalCtx['Elm'];

            if (typeof globalElm === "undefined") {
                globalCtx['Elm'] = Elm;
                return;
            }

            if (namespace in globalElm) {
                throw new Error('You have exported two Elm modules called `' + publicModule + '` on this page! Rename one of them.');
            }

            globalElm[namespace] = Elm[namespace];
        };
    };
};
