'use strict';

var fs = require('fs');
var path = require('path');
var marked = require('marked');

module.exports = function(grunt) {

	grunt.registerMultiTask('create-website-news-rss', 'Creates RSS html files for the news.', function() {

		var self = this;

		var root_dir;
		if(path.isAbsolute(this.data.repo_root)) {
			root_dir = path.normalize(this.data.repo_root);
		} else {
			root_dir = path.normalize(path.join(path.dirname(__dirname), this.data.repo_root));
		}


		/* MAIN FUNCTION */

		this.build = function() {

			// load earlier create array of news posts
			var news = grunt.file.readJSON(self.data.input.news);

			// iterate through news posts and handle them
			news.forEach(function(post) {
				self.handleNewsPost(post);
			});

		};


		/* HELPING FUNCTIONS */

		this.handleNewsPost = function(post) {
			// read content
			var content = fs.readFileSync(path.join(root_dir, post.file)).toString();

			// filter guid from news post
			var guid = (new RegExp(self.data.regex.guid, 'm')).exec(content);
			if(guid === null || guid.length < 2) {
				grunt.log.error('Could not find `guid` in news, cannot create RSS file.');
			}
			guid = guid[1]; // first capture group

			// parse markdown into html
			content = marked(content);

			// put content into html template
			content = '<!doctype html public "-//W3C//DTD HTML 4.0 Transitional //EN"><html><head>' +
					  '<meta http-equiv="Content-Type" content="text/html; charset-us-ascii"></head><body>' +
					  content +
					  '</body></html>';

			// write rss news html to disk
			grunt.file.write(path.join(self.data.output, guid + '.html'), content);
		};


		// finally, run the build!
		this.build();

	});

};