'use strict';

// module Web.Giflib.Internal.Debug

exports.log = function (s) {
    return function () {
        console.log(s);
    };
};
