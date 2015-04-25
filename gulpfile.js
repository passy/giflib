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
    var pscMake = purescript.psc({
        main: true,
        noOpts: false,
        noMagicDo: false,
        output: 'app.js'
    });

    pscMake.on('error', function(e) {
        console.error(e.message);
        pscMake.end();
    });

    return gulp.src([
        'app/purs/*.purs',
        'app/purs/**/*.purs',
        'bower_components/purescript-*/src/**/*.purs',
    ]).pipe(pscMake)
    .pipe(browserify({}))
    .pipe(gulp.dest('app/js/'));
};

gulp.task('build', build.bind(null, swallowError));

gulp.task('clean', del.bind(null, ['tmp', 'purs/js/app.js']));

gulp.task('watch', function () {
    gulp.watch(['app/purs/*.purs', 'app/purs/**/*.purs'], ['build']);
});

gulp.task('default', ['clean', 'build']);
