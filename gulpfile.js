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

gulp.task('copy-bower', function () {
    return gulp.src([
        'bower_components/**',
    ], { base: '.' })
        .pipe(gulp.dest('dist/'));
});

gulp.task('copy-static', function () {
    return gulp.src([
        'public/*',
        'public/**',
    ]).pipe(gulp.dest('dist/'));
});

gulp.task('clean', function () {
    return del.bind(null, ['dist']);
});

gulp.task('build', ['clean', 'copy-bower', 'copy-static', 'compile']);
gulp.task('default', ['build']);
