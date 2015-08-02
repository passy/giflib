'use strict';

// module Web.Firebase

exports.newFirebaseImpl = function (uri) {
    return function () {
        return new Firebase(uri);
    };
};

exports.childImpl = function (childPath, firebase) {
    return function () {
        return firebase.child(childPath);
    };
};

exports.onImpl = function (eventType, callback, cancelCallback, fb) {
    return function () {
        return fb.on(eventType, callback, cancelCallback);
    };
};

exports.setImpl = function (value, onComplete, fb) {
    return function () {
        fb.set(value, onComplete === null ? undefined : onComplete);
    };
};

exports.pushImpl = function (value, onComplete, fb) {
    return function () {
        fb.push(value, onComplete === null ? undefined : onComplete);
    };
};
