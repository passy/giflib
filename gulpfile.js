'use scrict';

var gulp = require('gulp');
var del = require('del');
var purescript = require('gulp-purescript');

gulp.task('build', function () {
    gulp.src([
        'purs/*.purs',
        'bower_components/purescript-*/src/**/*.purs',
    ]).pipe(purescript.psc({
        output: 'app.js',
        main: true
    }))
    .pipe(gulp.dest('js/'));
});

gulp.task('clean', del.bind(null, 'js'));

gulp.task('watch', function () {
    gulp.watch('purs/*.purs', ['build']);
});

gulp.task('default', ['clean', 'build']);
