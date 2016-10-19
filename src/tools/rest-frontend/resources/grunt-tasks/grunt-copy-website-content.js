'use strict';

var fs = require('fs-extra');
var path = require('path');

module.exports = function(grunt) {

	grunt.registerMultiTask('copy-website-content', 'Copies website content to a specific target directory.', function() {

		var self = this;

		var root_dir = path.normalize(path.join(path.dirname(__dirname), this.data.repo_root));
		var target_dir = path.normalize(path.join(path.dirname(__dirname), this.data.target_dir));


		/* MAIN FUNCTION */

		this.build = function() {

			// remove old website content entirely and/or create dir
			fs.emptyDirSync(target_dir);

			// read the input file
			// root structure is array holding objects
			var input = grunt.file.readJSON(self.data.input);

			// iterate through menu points and handle them
			input.forEach(function(entry) {
				self.handleMenuEntry(entry);
			});

			// print success message
			grunt.log.ok('Website content copied to target directory successfully!');

		};


		/* HELPING FUNCTIONS */

		this.handleMenuEntry = function(entry) {
			switch(entry.type) {
				case 'submenu':
				case 'listfiles':
					self.handleEntryWithChildren(entry);
					break;
				case 'file':
					self.handleFileEntry(entry);
					break;
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
			content = self.ensureProperFileContentFormat(path.basename(file), content);
			fs.writeFileSync(file, content);
		};

		this.ensureProperFileContentFormat = function(filename, content) {
			grunt.log.writeln(content);
			switch(path.extname(filename)) {
				case '':
				case '.md':
					content = self.replaceTabBySpaces(content);
					break;
				default: // code files
					content = self.replaceTabBySpaces(content);
					content = '```\n' + content + '\n```';
					break;
			}
			return content;
		};

		this.replaceTabBySpaces = function(text) {
			return text.replace(new RegExp('\t', 'g'), '    ');
		};


		// finally, run the build!
		this.build();

	});

};