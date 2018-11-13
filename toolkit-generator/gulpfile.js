const gulp = require("gulp");
const sass = require("gulp-sass");
const del = require("del");
const rename = require("gulp-rename");
const autoprefixer = require("gulp-autoprefixer");
const cleanCSS = require("gulp-clean-css");
const browserSync = require("browser-sync").create();
const concat = require("gulp-concat");

const source_dir = "./src";
const destination_dir = "./dist";

function styles() {
  return gulp
    .src(source_dir + "/toolkit/index.sass")
    .pipe(sass().on("error", sass.logError))
    .pipe(autoprefixer())
    .pipe(cleanCSS({ level: 2 }))
    .pipe(rename({
      basename: 'toolkit',
      suffix: '.min'
    }))
    .pipe(gulp.dest(destination_dir));
}

function initServer() {
  browserSync.init({
    server: {
      baseDir: './'
    }
  });
}

function clean() {
  return del([ destination_dir ]);
}

function watch() {
  gulp.watch(source_dir + '/**/*.(sass|scss)', () => {
    return styles().pipe(browserSync.stream());
  });
  gulp.watch('./index.html')
      .on('change', browserSync.reload);
}

const toolkit = gulp.series(clean, styles);

const devcards = gulp.series(
  toolkit,
  gulp.parallel(initServer, watch)
);

gulp.task('build', toolkit);
gulp.task('default', devcards);
