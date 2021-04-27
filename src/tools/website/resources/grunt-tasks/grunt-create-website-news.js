"use strict";

var fs = require("fs");
var path = require("path");
var slugify = require("slugify");

var resolve_path = require("./helper/resolve-path");

module.exports = function (grunt) {
  grunt.registerMultiTask(
    "create-website-news",
    "Builds a json file containing news information.",
    function () {
      var self = this;

      var root_dir = resolve_path(this.data.repo_root);
      var output_file = resolve_path(this.data.output);

      /* MAIN FUNCTION */

      this.build = function () {
        var result = [];

        // get all news files
        var posts = self
          .listNewsDirectory(self.data.news_root)
          .filter(function (post) {
            return post.stats.isFile();
          });

        // sort news files descending
        posts.sort(function (left, right) {
          // return value: negative value = left before right
          return Date.parse(left.date) > Date.parse(right.date) ? -1 : 1;
        });

        posts.forEach(function (post) {
          var regex_title = new RegExp(
            self.data.regex.title.pattern,
            self.data.regex.title.flags
          );
          var regex_shortdesc = new RegExp(
            self.data.regex.shortdesc.pattern,
            self.data.regex.shortdesc.flags
          );
          var title, content, shortDesc;

          // read content
          content = fs
            .readFileSync(path.join(root_dir, self.data.news_root, post.name))
            .toString();

          // filter title
          title = regex_title.exec(content);
          if (title !== null && title.length >= 2) {
            title = title[1];
          } else {
            // no title, warn
            title = "";
            grunt.log.error(
              "News post " +
                post.name +
                " has no title matching the configured regex!"
            );
          }

          // filter short description
          shortDesc = regex_shortdesc.exec(content);
          if (shortDesc !== null && shortDesc.length >= 2) {
            shortDesc = shortDesc[1];
          } else {
            shortDesc = "";
            grunt.log.warn(
              "News post " +
                post.name +
                " has no short description matching the configured regex!"
            );
          }

          result.push({
            file: path.join(self.data.news_root, post.name),
            date: post.date,
            title: title,
            shortDesc: shortDesc,
            slug: slugify(title.toLowerCase()),
            type: "file",
          });
        });

        // add sections for years
        result = self.addYearSections(result);

        grunt.file.write(output_file, JSON.stringify(result, null, 4));

        grunt.log.ok("Website news file generated successfully!");
      };

      this.listNewsDirectory = function (relPath) {
        var result = [];
        var regex = new RegExp(
          self.data.regex.filename.pattern,
          self.data.regex.filename.flags
        );
        var entries = fs.readdirSync(path.join(root_dir, relPath));
        var date;
        entries.forEach(function (entry) {
          date = regex.exec(entry);
          if (date === null || date.length < 2) {
            grunt.log.warn(
              "Found file `" +
                entry +
                "`, which has an inappropriate filename, in the news folder."
            );
          } else {
            date = date[1]; // first capture group is date
            var stat = fs.statSync(path.join(root_dir, relPath, entry));
            if (stat.isFile()) {
              result.push({ name: entry, date: date, stats: stat });
            }
          }
        });
        return result;
      };

      this.addYearSections = function (posts) {
        var result = [];

        var currDate,
          currYear = -1;
        posts.forEach(function (post) {
          currDate = new Date(Date.parse(post.date));
          if (currDate.getFullYear() !== currYear) {
            result.push({ name: currDate.getFullYear(), type: "section" });
            currYear = currDate.getFullYear();
          }
          result.push(post);
        });

        return result;
      };

      // finally, run the build!
      this.build();
    }
  );
};
