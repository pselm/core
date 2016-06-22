/* global exports */
"use strict";

// module Elm.Date

exports.fromStringImpl = function (str) {
    return new Date(str);
};

exports.fromTime = function (time) {
    return new Date(time);
};
