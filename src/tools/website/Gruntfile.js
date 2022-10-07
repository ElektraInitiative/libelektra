var modRewrite = require("connect-modrewrite");
var serveStatic = require("serve-static");

module.exports = function (grunt) {
  var dstFileBanner =
    '/**\n * @application <%= pkg.name %>\n * @version <%= pkg.version %>\n * @updated <%= grunt.template.today("yyyy-mm-dd") %>\n * @author <%= pkg.author %>\n * @license <%= pkg.license %>\n */\n';

  // Project configuration.
  grunt.initConfig({
    app: grunt.file.readJSON("application-config.json"),
    pkg: grunt.file.readJSON("package.json"),
    global: {
      repository: { root: "@REST_FRONTEND_SOURCE_INSTALL_REPOSITORY@" },
    },
    "create-website-structure": {
      options: {},
      build: {
        repo_root: "<%= global.repository.root %>",
        input: "resources/structure.json.in",
        output: "resources/structure.json",
      },
    },
    "create-website-news": {
      options: {},
      build: {
        repo_root: "<%= global.repository.root %>",
        news_root: "doc/news",
        output: "resources/news.json",
        regex: {
          filename: {
            pattern: "^([0-9]{4}\\-[0-9]{2}\\-[0-9]{2})_(.*)$",
            flags: "i",
          },
          title: { pattern: "^# ([^#\n]*)$", flags: "im" },
          shortdesc: { pattern: "^- shortDesc: (.*)$", flags: "im" },
        },
      },
    },
    "create-website-news-rss": {
      options: {},
      build: {
        repo_root: "<%= global.repository.root %>",
        input: {
          news: "<%= grunt.config('create-website-news.build.output') %>",
        },
        regex: { guid: { pattern: "^\\- guid: ([a-fA-F0-9-]+)$", flags: "m" } },
        feed: {
          title: "Elektra's Blog",
          description: "News around Elektra",
          feed_url:
            "<%= app.website.url %>news/<%= grunt.config('create-website-news-rss.build.output.feed') %>",
          post_url: "<%= app.website.url %>news/",
          site_url: "<%= app.website.url %>",
          language: "en",
          pubDate: new Date().toUTCString(),
          ttl: 1800,
        },
        output: { dir: "public/news", feed: "feed.rss" },
      },
    },
    "copy-website-content": {
      options: {},
      build: {
        repo_root: "<%= global.repository.root %>",
        input: {
          structure:
            "<%= grunt.config('create-website-structure.build.output') %>",
          news: "<%= grunt.config('create-website-news.build.output') %>",
        },
        target_dir: "public/website",
      },
    },
    jshint: {
      options: {
        reporter: require("jshint-stylish"),
        node: true,
        browserify: true,
      },
      build: ["Gruntfile.js", "resources/assets/js/**/*.js"],
    },
    less: {
      options: {},
      build: {
        files: {
          "public/assets/skin/default/css/theme.css":
            "resources/assets/skin/default/less/theme.less",
          "public/assets/skin/bootstrap/bootstrap.css":
            "resources/assets/skin/bootstrap/bootstrap.less",
        },
      },
    },
    cssmin: {
      options: { banner: dstFileBanner },
      build: {
        files: {
          "public/assets/skin/default/css/theme.min.css":
            "public/assets/skin/default/css/theme.css",
          "public/assets/skin/bootstrap/bootstrap.min.css":
            "public/assets/skin/bootstrap/bootstrap.css",
        },
      },
    },
    concat: {
      vendor: {
        options: { banner: dstFileBanner },
        src: [
          "public/assets/skin/bootstrap/bootstrap.min.css",
          "node_modules/highlight.js/styles/github.css",
          "node_modules/angular-ui-notification/dist/angular-ui-notification.min.css",
          "node_modules/ng-tags-input/build/ng-tags-input.min.css",
          "node_modules/ng-tags-input/build/ng-tags-input.bootstrap.min.css",
          "node_modules/angular-typewriter/npm-dist/angular-typewrite.css",
        ],
        dest: "public/assets/skin/vendor.css",
      },
      docsearchcss: {
        src: ["node_modules/docsearch.js/dist/cdn/docsearch.min.css"],
        dest: "public/assets/skin/docsearch.min.css",
      },
      docsearchcssmap: {
        src: ["node_modules/docsearch.js/dist/cdn/docsearch.min.css.map"],
        dest: "public/assets/skin/docsearch.min.css.map",
      },
      docsearchjs: {
        src: ["node_modules/docsearch.js/dist/cdn/docsearch.min.js"],
        dest: "public/vendor/docsearch.min.js",
      },
    },
    copy: {
      options: {},
      build: {
        files: [
          {
            cwd: "<%= global.repository.root %>/doc/images",
            src: [
              "qtgui.png",
              "web-gui-kdb.png",
              "oyranos-kolor-manager.png",
              "oyranos-km-logo.svg",
            ],
            dest: "<%= grunt.config('copy-website-content.build.target_dir') %>/img",
            expand: true,
          },
        ],
      },
    },
    preprocess: {
      options: {
        context: {
          CONFIGURATION: "<%= grunt.file.read('application-config.json') %>",
          WEBSTRUCTURE: "<%= grunt.file.read('resources/structure.json') %>",
          NEWS: "<%= grunt.file.read('resources/news.json') %>",
          BUILDDATE: new Date().toUTCString(),
        },
        type: "js",
      },
      config: {
        src: "resources/assets/js/config/index.js.in",
        dest: "resources/assets/js/config/index.js",
      },
      index: {
        src: "resources/assets/html/index.html.in",
        dest: "public/index.html",
      },
    },
    watch: {
      options: {},
      less: {
        files: ["resources/assets/skin/**/*"],
        tasks: ["less", "cssmin"],
      },
      preprocess: {
        files: [
          "application-config.json",
          "<%= grunt.config('create-website-structure.build.output') %>",
          "<%= grunt.config('create-website-news.build.output') %>",
        ],
        tasks: ["preprocess", "browserify:build"],
      },
      js: {
        files: ["public/assets/js/application.js"],
        tasks: [], // do nothing, the watcher only triggers browserify implicitly
      },
    },
    browserify: {
      options: {},
      build: {
        src: "resources/assets/js/application.js",
        dest: "resources/application.js.tmp",
        options: { banner: dstFileBanner, watch: true },
      },
    },
    uglify: {
      options: {
        compress: true,
        mangle: false,
        sourceMap: true,
        screwIE8: false,
      },
      build: {
        files: {
          "public/assets/js/application.js": [
            "<%= grunt.config('browserify.build.dest') %>",
          ],
        },
      },
    },
    connect: {
      server: {
        options: {
          hostname: "*", // * for any
          port: 9000,
          protocol: "http",
          base: {
            path: "public",
            options: { index: "index.html", maxAge: 3600 },
          },
          keepalive: true,
          middleware: function (connect, options, middlewares) {
            //                        middlewares.unshift(function(req, res, next) {
            //                            grunt.log.writeln('URL: ' + req.url);
            //                            if(!grunt.file.exists('public', req.url)) {
            //                                req.redirect('index.html');
            //                            }
            //                            next();
            //                        });
            //                        return middlewares;
            var staticExtensions = [
              "html",
              "js",
              "css",
              "json",
              "svg",
              "md",
              "png",
              "jpg",
              "gif",
              "otf",
              "eot",
              "ttf",
              "woff",
              "woff2",
              "xml",
              "c",
              "h",
              "cpp",
              "hpp",
              "java",
              "py",
              "rss",
              "ini",
            ];
            return [
              modRewrite([
                "!" +
                  staticExtensions
                    .map(function (elem) {
                      return "\\." + elem;
                    })
                    .join("|") +
                  "$ /index.html [L]",
              ]),
              serveStatic("public"),
            ];
          },
        },
      },
    },
  });

  grunt.loadNpmTasks("grunt-browserify");
  grunt.loadNpmTasks("grunt-contrib-concat");
  grunt.loadNpmTasks("grunt-contrib-connect");
  grunt.loadNpmTasks("grunt-contrib-copy");
  grunt.loadNpmTasks("grunt-contrib-cssmin");
  grunt.loadNpmTasks("grunt-contrib-jshint");
  grunt.loadNpmTasks("grunt-contrib-less");
  grunt.loadNpmTasks("grunt-contrib-uglify");
  grunt.loadNpmTasks("grunt-contrib-watch");
  grunt.loadNpmTasks("grunt-preprocess");

  grunt.loadTasks("./resources/grunt-tasks");

  grunt.registerTask("default", ["full"]);
  grunt.registerTask("full", [
    "stylesheets",
    "website-news",
    "create-website-structure",
    "copy-website-content",
    "copy",
    "preprocess",
    "browserify:build",
    "uglify:build",
  ]);
  grunt.registerTask("install", [
    "stylesheets",
    "website-news",
    "create-website-structure",
    "copy-website-content",
    "copy",
    "preprocess",
    "browserify:build",
    "uglify:build",
  ]);
  grunt.registerTask("stylesheets", ["less", "cssmin", "concat"]);
  grunt.registerTask("website-news", [
    "create-website-news",
    "create-website-news-rss",
  ]);
  grunt.registerTask("server", ["connect"]);
};
