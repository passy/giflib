'use strict';

// module Web.Giflib.Internal.Unsafe

exports.unsafeEvalEff = function (f) {
    f();
    return f;
};
