'use scrict';

var gulp = require('gulp');
var del = require('del');
var purescript = require('gulp-purescript');
var browserify = require('gulp-browserify');

gulp.task('build', function () {
    gulp.src([
        'app/purs/*.purs',
        'app/purs/**/*.purs',
        'bower_components/purescript-*/src/**/*.purs',
    ]).pipe(purescript.psc({
        output: 'app.js',
        main: true
    }))
    .pipe(browserify({}))
    .pipe(gulp.dest('app/js/'));
});

gulp.task('clean', del.bind(null, 'js/app.js'));

gulp.task('watch', function () {
    gulp.watch(['app/purs/*.purs', 'app/purs/**/*.purs'], ['build']);
});

gulp.task('default', ['clean', 'build']);
