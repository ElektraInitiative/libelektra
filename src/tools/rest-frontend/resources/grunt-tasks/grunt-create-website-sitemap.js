'use strict';

var builder = require('xmlbuilder');
var request = require('sync-request');

module.exports = function(grunt) {

	grunt.registerMultiTask('create-website-sitemap', 'Builds a sitemal.xml for the website.', function() {

		var self = this;


		/* MAIN FUNCTION */

		this.build = function() {

			// build XML string
			var urlset = builder.create('urlset', {
			  version: '1.0',
			  encoding: 'UTF-8'
			});

			// set important XML namespaces
			urlset.att('xmlns', 'http://www.sitemaps.org/schemas/sitemap/0.9')
				.att('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance')
				.att('xsi:schemaLocation',
					'http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd');

			// go through static paths
			var url;
			self.data.static.paths.forEach(function(path) {
			  url = urlset.ele('url');
			  url.ele('loc').txt(self.data.root_url + path);
	//		  url.ele('lastmod').txt((new Date()).toUTCString());
	//		  url.ele('changefreq').txt(self.data.static.change_frequency);
			});

			// go through structure file
			var structure = grunt.file.readJSON(self.data.dynamic.input);
			structure.forEach(function(elem) {
				self.handleMenuEntry(elem, urlset);
			});

			// go through entries in backend
			var response = request('GET', self.data.dynamic.backend + 'database');
			if(response.statusCode !== 200) {
				grunt.log.error('Could not reach Backend and could therefore not create sitemap.xml!');
			}
			var respObj = JSON.parse(response.getBody());
			respObj.entries.forEach(function(entry) {
				url = urlset.ele('url');
				url.ele('loc').txt(self.data.root_url + 'entries/details/' +
						encodeURIComponent(entry.key.full).replace(/%/g, '~'));
			});

			// format XML string
			var sitemap = urlset.end({
			  pretty: true,
			  indent: '  ',
			  newline: '\n'
			});

			grunt.file.write(self.data.output, sitemap);

		};

		this.handleMenuEntry = function(entry, urlset, parent) {
			switch(entry.type) {
				case 'submenu':
				case 'listfiles':
					self.handleEntryWithChildren(entry, urlset);
					break;
				case 'file':
					self.handleFileEntry(entry, urlset, parent);
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

		this.handleEntryWithChildren = function(entry, urlset) {
			entry.children.forEach(function(child) {
				switch(child.type) {
					case 'listfiles':
						var url;
						child.children.forEach(function(file) {
							if(file.type === 'file') {
								url = urlset.ele('url');
								url.ele('loc').txt(self.data.root_url + child.ref + '/' + file.slug);
							}
						});
						break;
					case 'file':
						self.handleFileEntry(child, urlset, entry);
						break;
					default:
						break;
				}
			});
		};

		this.handleFileEntry = function(entry, urlset, parent) {
			var url = urlset.ele('url');
			url.ele('loc').txt(self.data.root_url + parent.ref + '/' + entry.slug);
		};


		// finally, run the build!
		this.build();

	});

};