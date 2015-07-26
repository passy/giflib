'use strict';

// module Data.Int.Extra.Unsafe

exports.unsafeFromNumberImpl = function (just) {
  return function (nothing) {
    return function (n) {
      var i = parseInt(n, 10);
      return i === n ? just(n) : nothing;
    };
  };
};
