'use strict';

var fs = require('fs');
var path = require('path');
var marked = require('marked');

var resolve_path = require('./resolve-path');

module.exports = function(grunt) {

	grunt.registerMultiTask('create-website-news-rss', 'Creates RSS html files for the news.', function() {

		var self = this;

		var root_dir		= resolve_path(this.data.repo_root);
		var input_news_file	= resolve_path(this.data.input.news);
		var output_dir		= resolve_path(this.data.output);


		/* MAIN FUNCTION */

		this.build = function() {

			// load earlier create array of news posts
			var news = grunt.file.readJSON(input_news_file);

			// iterate through news posts and handle them
			news.forEach(function(post) {
				self.handleNewsPost(post);
			});

			grunt.log.ok('Website RSS news generated successfully!');

		};


		/* HELPING FUNCTIONS */

		this.handleNewsPost = function(post) {
			// read content
			var content = fs.readFileSync(path.join(root_dir, post.file)).toString();

			// filter guid from news post
			var guid = (new RegExp(self.data.regex.guid.pattern, self.data.regex.guid.flags)).exec(content);
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
			grunt.file.write(path.join(output_dir, guid + '.html'), content);
		};


		// finally, run the build!
		this.build();

	});

};