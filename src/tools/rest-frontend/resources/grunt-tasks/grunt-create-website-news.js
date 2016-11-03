'use strict';

var fs = require('fs');
var path = require('path');
var slugify = require('slugify');

var resolve_path = require('./helper/resolve-path');

module.exports = function(grunt) {

	grunt.registerMultiTask('create-website-news', 'Builds a json file containing news information.', function() {

		var self = this;

		var root_dir	= resolve_path(this.data.repo_root);
		var output_file = resolve_path(this.data.output);


		/* MAIN FUNCTION */

		this.build = function() {

			var result = [];

			// get all news files
			var posts = self.listNewsDirectory(self.data.news_root).filter(function(post) {
				return post.stats.isFile();
			});

			// sort news files descending
			posts.sort(function(left, right) {
				// return value: negative value = left before right
				return (Date.parse(left.date) > Date.parse(right.date)) ? -1 : 1;
			});

			posts.forEach(function(post) {
				var regex = new RegExp(self.data.regex.title.pattern, self.data.regex.title.flags);
				var title, content;
				content = fs.readFileSync(path.join(root_dir, self.data.news_root, post.name)).toString().split('\n');
				for(var i = 0; i < content.length; i++) {
					title = regex.exec(content[i]);
					if(title !== null && title.length >= 2) {
						title = title[1];
						break;
					}
				}
				result.push({
					file: path.join(self.data.news_root, post.name),
					date: post.date,
					title: title,
					slug: slugify(title.toLowerCase())
				});
			});

			grunt.file.write(output_file, JSON.stringify(result, null, 4));

			grunt.log.ok('Website news file generated successfully!');

		};

		this.listNewsDirectory = function(relPath) {
			var result = [];
			var regex = new RegExp(self.data.regex.filename.pattern, self.data.regex.filename.flags);
			var entries = fs.readdirSync(path.join(root_dir, relPath));
			var date;
			entries.forEach(function(entry) {
				date = regex.exec(entry);
				if(date === null || date.length < 2) {
					grunt.log.error('Found news file with inappropriate filename.');
				} else {
					date = date[1]; // first capture group is date
				}
				var stat = fs.statSync(path.join(root_dir, relPath, entry));
				if(stat.isFile()) {
					result.push({
						name: entry,
						date: date,
						stats: stat
					});
				}
			});
			return result;
		};


		// finally, run the build!
		this.build();

	});

};