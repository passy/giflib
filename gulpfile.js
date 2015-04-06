'use scrict';

var gulp = require('gulp');
var del = require('del');
var purescript = require('gulp-purescript');
var browserify = require('gulp-browserify');

var swallowError = function (error) {
    console.log(error.toString());
    this.emit('end');
};

var noop = function () {
};

var build = function (errorHandler) {
    return gulp.src([
        'app/purs/*.purs',
        'app/purs/**/*.purs',
        'bower_components/purescript-*/src/**/*.purs',
    ]).pipe(purescript.psc({
        output: 'app.js',
        main: true
    }))
    .on('error', errorHandler || noop)
    .pipe(browserify({}))
    .pipe(gulp.dest('app/js/'));
};

gulp.task('build', build);
gulp.task('buildSwallow', build.bind(null, swallowError));

gulp.task('clean', del.bind(null, 'js/app.js'));

gulp.task('watch', function () {
    gulp.watch(['app/purs/*.purs', 'app/purs/**/*.purs'], ['buildSwallow']);
});

gulp.task('default', ['clean', 'build']);
