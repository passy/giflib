'use strict';

// module Web.Giflib.Internal.Unsafe

exports.unsafePerformEff = function (f) {
    return f();
};


exports.showForeign = function (a) {
    return JSON.stringify(a);
};

exports.unsafeEvalEff = function (f) {
    f();
    return f;
};

exports.unsafeLog = function (x) {
    console.log(x);
    return x;
};
