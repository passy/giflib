'use scrict';

var gulp = require('gulp');
var del = require('del');
var proc = require('child_process');
var fs = require('fs');
var mkdirp = require('mkdirp');

gulp.task('compile', function () {
    mkdirp.sync('dist/js/');
    var out = proc.execSync('node_modules/.bin/pulp browserify');
    fs.writeFileSync('dist/js/app.js', out);
});

gulp.task('copy', function () {
    return gulp.src([
        'bower_components',
        'public'
    ], { base: '.' })
        .pipe(gulp.dest('dist/'));
});

gulp.task('clean', function () {
    return del.bind(null, ['dist']);
});

gulp.task('build', ['clean', 'copy', 'compile']);
gulp.task('default', ['build']);
