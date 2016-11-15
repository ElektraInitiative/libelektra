'use strict';

var fs = require('fs-extra');
var path = require('path');

var resolve_path = require('./helper/resolve-path');

module.exports = function(grunt) {

	grunt.registerMultiTask('copy-website-content', 'Copies website content to a specific target directory.', function() {

		var self = this;

		var root_dir				= resolve_path(this.data.repo_root);
		var target_dir				= resolve_path(this.data.target_dir);
		var input_structure_file	= resolve_path(this.data.input.structure);
		var input_news_file			= resolve_path(this.data.input.news);


		/* MAIN FUNCTION */

		this.build = function() {

			// remove old website content entirely and/or create dir
			fs.emptyDirSync(target_dir);

			// read the structure input file
			// root structure is array holding objects
			var structure = grunt.file.readJSON(input_structure_file);

			// read the news input file
			// root structure is array holding objects
			var news = grunt.file.readJSON(input_news_file);

			// iterate through menu points and handle them
			structure.forEach(function(entry) {
				self.handleMenuEntry(entry);
			});

			news.forEach(function(post) {
				self.handleNewsPost(post);
			});

			// print success message
			grunt.log.ok('Website content copied to target directory successfully!');

		};


		/* HELPING FUNCTIONS */

		this.handleNewsPost = function(post) {
			var file = path.join(target_dir, post.file);
			var dir = path.dirname(file);
			// first create dir
			if(!fs.existsSync(dir)) {
				fs.ensureDirSync(dir);
			}
			// now read and copy
			var content = fs.readFileSync(path.join(root_dir, post.file), {
				encoding: 'utf8'
			});
			content = self.ensureProperFileContentFormat(post.file, content);
			fs.writeFileSync(file, content);
		};

		this.handleMenuEntry = function(entry) {
			switch(entry.type) {
				case 'submenu':
				case 'listfiles':
					self.handleEntryWithChildren(entry);
					break;
				case 'file':
					self.handleFileEntry(entry);
					break;
				case 'ref':
				case 'link':
				case 'section':
					// nothing to do
					break;
				default:
					grunt.log.error('The menu type "' + entry.type + '" is not supported.');
					break;
			}
		};

		this.handleEntryWithChildren = function(entry) {
			entry.children.forEach(function(child) {
				self.handleMenuEntry(child);
			});
		};

		this.handleFileEntry = function(entry) {
			var file = path.join(target_dir, entry.options.path);
			var dir = path.dirname(file);
			// first create dir
			if(!fs.existsSync(dir)) {
				fs.ensureDirSync(dir);
			}
			// now read and copy
			var content = fs.readFileSync(path.join(root_dir, entry.options.path), {
				encoding: 'utf8'
			});
			content = self.ensureProperFileContentFormat(entry.options.path, content);
			fs.writeFileSync(file, content);
		};

		this.ensureProperFileContentFormat = function(filepath, content) {
			switch(path.extname(filepath)) {
				case '':
				case '.md':
					content = self.replaceTabBySpaces(content);
					content = self.ensureAbsoluteLinkPaths(filepath, content);
					content = self.ensureLinkToFile(content);
					break;
				default: // code files
					content = self.replaceTabBySpaces(content);
					content = '```\n' + content + '\n```';
					break;
			}
			switch(path.basename(filepath)) {
				case 'README.md':
					content = self.reformatReadmeInfoBlock(content);
					break;
				default:
					break;
			}
			return content;
		};

		this.replaceTabBySpaces = function(text) {
			return text.replace(new RegExp('\t', 'g'), '    ');
		};

		this.reformatReadmeInfoBlock = function(text) {
			var lines = text.split('\n');
			// iterate to last infos line
			var lastInfoLine = 0;
			for(var i = 0; i < lines.length; i++) {
				if(lines[i].indexOf('- infos') > -1) {
					lastInfoLine = i;
					continue;
				}
			}
			if(lastInfoLine > 0) {
				lines.splice(lastInfoLine + 1, 0, '```');
				lines.splice(0, 0, '```');
			}
			return lines.join('\n');
		};

		this.ensureAbsoluteLinkPaths = function(filepath, text) {
			return text.replace(/\[(.+)\]\(([^\)]+)\)/gi, function(match, text, url) {
				if(url.indexOf('://') > -1 || url.charAt(0) === '#') {
					return match;
				} else {
					if(url.charAt(0) === '/') {
						return '[' + text + '](' + url.substr(1) + ')';
					} else {
						return '[' + text + '](' + path.join(path.dirname(filepath), url) + ')';
					}
				}
			});
		};

		this.ensureLinkToFile = function(text) {
			return text.replace(/\[(.+)\]\(([^\)]+)\)/gi, function(match, text, url) {
				if(url.indexOf('://') > -1 || url.charAt(0) === '#') {
					return match;
				} else {
					var file = path.normalize(path.join(root_dir, url));
					try {
						if(fs.statSync(file).isDirectory()) {
							var target_files = grunt.config().app.website.target_file_dir_links;
							for(var i = 0; i < target_files.length; i++) {
								try {
									if(fs.statSync(path.join(file, target_files[i])).isFile()) {
										return '[' + text + '](' + path.join(url, target_files[i]) + ')';
									}
								} catch (error) {
									// do nothing
								}
							}
						}
						return match;
					} catch (error) {
						return match;
					}
				}
			});
		};


		// finally, run the build!
		this.build();

	});

};