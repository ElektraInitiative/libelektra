"use strict";

var fs = require("fs");
var path = require("path");
var marked = require("marked");
var RSS = require("rss");

var resolve_path = require("./helper/resolve-path");

module.exports = function (grunt) {
  grunt.registerMultiTask(
    "create-website-news-rss",
    "Creates RSS html files for the news.",
    function () {
      var self = this;

      var root_dir = resolve_path(this.data.repo_root);
      var input_news_file = resolve_path(this.data.input.news);
      var output_dir = resolve_path(this.data.output.dir);

      /* MAIN FUNCTION */

      this.build = function () {
        // load earlier create array of news posts
        var news = grunt.file.readJSON(input_news_file);

        // prepare RSS feed
        var feed = new RSS(self.data.feed);

        // iterate through news posts and handle them
        news.forEach(function (post) {
          if (post.type === "file") {
            self.handleNewsPost(post, feed);
          }
        });

        // save feed
        var xml = feed.xml({ indent: true });
        grunt.file.write(path.join(output_dir, self.data.output.feed), xml);

        grunt.log.ok("Website RSS news generated successfully!");
      };

      /* HELPING FUNCTIONS */

      this.handleNewsPost = function (post, feed) {
        // read content
        var content = fs
          .readFileSync(path.join(root_dir, post.file))
          .toString();

        // filter guid from news post
        var guid = new RegExp(
          self.data.regex.guid.pattern,
          self.data.regex.guid.flags
        ).exec(content);
        if (guid === null || guid.length < 2) {
          grunt.log.warn(
            "News post " +
              post.name +
              " has no guid that can be used to generate an RSS post!"
          );
          return; // no guid, no RSS!
        }
        guid = guid[1]; // first capture group

        // parse markdown into html
        content = marked.parse(content);

        // put content into html template
        content =
          '<!doctype html public "-//W3C//DTD HTML 4.0 Transitional //EN"><html><head>' +
          '<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"></head><body>' +
          content +
          "</body></html>";

        // write rss news html to disk
        grunt.file.write(path.join(output_dir, guid + ".html"), content);

        // add post to rss feed
        feed.item({
          title: post.title,
          description: content,
          url: self.data.feed.post_url + guid + ".html",
          guid: guid,
          date: new Date(Date.parse(post.date)).toUTCString(),
        });
      };

      // finally, run the build!
      this.build();
    }
  );
};
