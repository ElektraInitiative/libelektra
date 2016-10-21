module.exports = function(grunt) {

    var dstFileBanner = '/**\n * @application <%= pkg.name %>\n * @version <%= pkg.version %>\n * @updated <%= grunt.template.today("yyyy-mm-dd") %>\n * @author <%= pkg.author %>\n * @license <%= pkg.license %>\n */\n';

    // Project configuration.
    grunt.initConfig({
        pkg: grunt.file.readJSON('package.json'),
        'create-website-structure': {
            options: { },
            build: {
				repo_root: '../../../..',
				input: 'resources/structure.json.in',
                output: 'resources/structure.json'
            }
		},
		'copy-website-content': {
			options: { },
			build: {
				repo_root: '../../../..',
				input: 'resources/structure.json',
				target_dir: '../public/website'
			}
		},
        jshint: {
            options: {
                reporter: require('jshint-stylish'),
				node: true
            },
            build: ['Gruntfile.js', 'resources/assets/js/**/*.js']
        },
        less: {
			options: { },
            build: {
                files: {
                    'public/assets/skin/default/css/theme.css': 'resources/assets/skin/default/less/theme.less'
                }
            }
        },
        cssmin: {
            options: {
                banner: dstFileBanner
            },
            build: {
                files: {
                    'public/assets/skin/default/css/theme.min.css': 'public/assets/skin/default/css/theme.css'
                }
            }
        },
		concat: {
			options: {
				banner: dstFileBanner
			},
			vendor: {
				src: [
					'node_modules/bootstrap/dist/css/bootstrap.min.css',
					'node_modules/bootstrap/dist/css/bootstrap-theme.min.css',
					'node_modules/highlight.js/styles/github.css',
					'node_modules/angular-ui-notification/dist/angular-ui-notification.min.css',
					'node_modules/ng-tags-input/build/ng-tags-input.min.css',
					'node_modules/ng-tags-input/build/ng-tags-input.bootstrap.min.css'
				],
				dest: 'public/assets/skin/vendor.css',
			},
			pacejs: {
				src: ['node_modules/pace-progress/pace.min.js'],
				dest: 'public/vendor/pace.min.js'
			},
			pacecss: {
				src: ['node_modules/pace-progress/themes/blue/pace-theme-minimal.css'],
				dest: 'public/vendor/pace.min.css'
			}
		},
        preprocess: {
            options: {
                context: {
                    CONFIGURATION: grunt.file.read('application-config.json'),
					WEBSTRUCTURE: grunt.file.read('resources/structure.json')
                },
				type: 'js'
            },
            config: {
                src: 'resources/assets/js/config/index.js.in',
                dest: 'resources/assets/js/config/index.js'
            }
        },
        watch: {
			options: { },
            less: {
                files: ['resources/assets/skin/**/*'],
                tasks: ['less','cssmin']
            },
			preprocess: {
				files: [
					'resources/assets/js/config/constants.config.js.in',
					'application-config.json',
					'resources/structure.json'
				],
				tasks: ['preprocess', 'browserify:build']
			}
        },
		browserify: {
			options: { },
			build: {
				src: 'resources/assets/js/application.js',
				dest: 'public/assets/js/application.js',
				options: {
					banner: dstFileBanner,
					require: [],
					external: [],
					watch: true,
					keepAlive: true
				}
			}
		},
        'http-server': {

            dev: {

                // the server root directory
                root: 'public',

                // the server port
                port: 9000,

                // the host ip address
                // If specified to, for example, "127.0.0.1" the server will
                // only be available on that ip.
                // Specify "0.0.0.0" to be available everywhere
                host: "0.0.0.0",

                cache: 0,
                showDir : false,
                autoIndex: true,

                // server default file extension
                ext: "html",

                // run in parallel with other tasks
                runInBackground: false,

                // specify a logger function. By default the requests are
                // sent to stdout.
                logFn: function(req, res, error) { },

                // Proxies all requests which can't be resolved locally to the given url
                // Note this this will disable 'showDir'
                //proxy: "http://someurl.com",

                /// Use 'https: true' for default module SSL configuration
                /// (default state is disabled)
                //https: {
                //    cert: "cert.pem",
                //    key : "key.pem"
                //},

                // Tell grunt task to open the browser
                //openBrowser : false,

                // customize url to serve specific pages
                //customPages: {
                //    "/node_modules": "node_modules"
                //}

            }

        }
    });

    grunt.loadNpmTasks('grunt-contrib-jshint');
    grunt.loadNpmTasks('grunt-contrib-less');
    grunt.loadNpmTasks('grunt-contrib-cssmin');
    grunt.loadNpmTasks('grunt-contrib-watch');
	grunt.loadNpmTasks('grunt-contrib-concat');
	grunt.loadNpmTasks('grunt-preprocess');
    grunt.loadNpmTasks('grunt-http-server');
	grunt.loadNpmTasks('grunt-browserify');

    grunt.loadTasks('./resources/grunt-tasks');

    grunt.registerTask('default', ['full']);
    grunt.registerTask('full', [
		'less', 'cssmin', 'concat', 'create-website-structure', 'copy-website-content', 'preprocess', 'browserify:build'
	]);
    grunt.registerTask('server', ['http-server']);

};
