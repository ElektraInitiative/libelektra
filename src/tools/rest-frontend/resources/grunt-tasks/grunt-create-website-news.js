'use strict';

var fs = require('fs');
var path = require('path');
var slugify = require('slugify');

module.exports = function(grunt) {

	grunt.registerMultiTask('create-website-news', 'Builds a json file containing news information.', function() {

		var self = this;

		var root_dir;
		if(path.isAbsolute(this.data.repo_root)) {
			root_dir = path.normalize(this.data.repo_root);
		} else {
			root_dir = path.normalize(path.join(path.dirname(__dirname), this.data.repo_root));
		}


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
				var regex = new RegExp(self.data.title_regex, 'i');
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

			grunt.file.write(self.data.output, JSON.stringify(result, null, 4));

		};

		this.listNewsDirectory = function(relPath) {
			var result = [];
			var regex = new RegExp(self.data.filename_regex, 'i');
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