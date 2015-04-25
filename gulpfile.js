'use scrict';

var gulp = require('gulp');
var del = require('del');
var purescript = require('gulp-purescript');
var browserify = require('gulp-browserify');

var swallowError = function (error) {
    console.error(error.toString());
    this.emit('end');
};

var build = function (errorHandler) {
    return gulp.src([
        'app/purs/*.purs',
        'app/purs/**/*.purs',
        'bower_components/purescript-*/src/**/*.purs',
    ]).pipe(purescript.pscMake({
        output: 'tmp',
        main: true,
        noOpts: true,
        noMagicDo: true
    }))
    .on('error', errorHandler)
    .pipe(browserify({}))
    .pipe(gulp.dest('app/js/'));
};

gulp.task('build', build.bind(null, swallowError));

gulp.task('clean', del.bind(null, ['tmp', 'js/app.js']));

gulp.task('watch', function () {
    gulp.watch(['app/purs/*.purs', 'app/purs/**/*.purs'], ['build']);
});

gulp.task('default', ['clean', 'build']);
