"use strict";

var fs = require("fs");
var path = require("path");

var resolve_path = require("./helper/resolve-path");

module.exports = function (grunt) {
  grunt.registerMultiTask(
    "create-website-structure",
    "Builds a comprehensive website structure file.",
    function () {
      var self = this;

      var root_dir = resolve_path(this.data.repo_root);
      var input_file = resolve_path(this.data.input);
      var output_file = resolve_path(this.data.output);

      /* MAIN FUNCTION */

      this.build = function () {
        // read the input file
        // root structure is array holding objects
        var input = grunt.file.readJSON(input_file);
        // create the output array
        var output = [];

        // iterate through menu points and handle them
        input.forEach(function (entry) {
          output.push(self.handleMenuEntry(entry));
        });

        // write result into output file
        grunt.file.write(output_file, JSON.stringify(output, null, 4));

        // print success message
        grunt.log.ok("Website structure file generated successfully!");
      };

      /* HELPING FUNCTIONS */

      this.handleMenuEntry = function (entry) {
        var result = {};
        switch (entry.type) {
          case "submenu":
            result = self.handleSubmenuEntry(entry);
            break;
          case "link":
            result = self.handleLinkEntry(entry);
            break;
          case "parsereadme":
            result = self.handleParsereadmeEntry(entry);
            break;
          case "listdirs":
            result = self.handleListdirsEntry(entry);
            break;
          case "listfiles":
            result = self.handleListfilesEntry(entry);
            break;
          case "staticlist":
            result = self.handleStaticlistEntry(entry);
            break;
          case "staticfile":
            result = self.handleStaticfileEntry(entry);
            break;
          case "staticref":
            result = self.handleStaticrefEntry(entry);
            break;
          case "section":
            result = self.handleSectionEntry(entry);
            break;
          case "parsefolders":
            result = self.handleParsefoldersEntry(entry);
            break;
          default:
            grunt.log.error(
              'The menu type "' + entry.type + '" is not supported.'
            );
            break;
        }
        return result;
      };

      this.handleSubmenuEntry = function (entry) {
        var result = {
          name: entry.name,
          type: "submenu",
          ref: entry.ref,
          children: [],
        };
        entry.children.forEach(function (child) {
          result.children.push(self.handleMenuEntry(child));
        });
        return result;
      };

      this.handleLinkEntry = function (entry) {
        var result = {
          name: entry.name,
          type: "link",
          ref: entry.ref,
          options: { path: entry.options.path },
        };
        return result;
      };

      this.handleParsereadmeEntry = function (entry) {
        var result = {
          name: entry.name,
          type: "listfiles",
          ref: entry.ref,
          children: [],
        };

        // pretty name option
        var pretty =
          typeof entry.options.name.make_pretty === "undefined"
            ? true
            : entry.options.name.make_pretty;

        var fileEntry = {
          name: self.makePrettyName(path.basename(entry.options.path)),
          type: "file",
          options: { path: entry.options.path },
        };
        fileEntry.slug = self.createSlugFromName(fileEntry.name);
        result.children.push(fileEntry);
        var readme = fs
          .readFileSync(path.join(root_dir, entry.options.path))
          .toString()
          .split("\n");
        var hasToParse = false;
        readme.forEach(function (line) {
          if (
            typeof entry.options.parsing.start_regex === "undefined" ||
            (typeof entry.options.parsing.start_regex !== "undefined" &&
              line.match(new RegExp(entry.options.parsing.start_regex, "i")))
          ) {
            hasToParse = true;
          }
          if (
            typeof entry.options.parsing.stop_regex !== "undefined" &&
            line.match(new RegExp(entry.options.parsing.stop_regex, "i"))
          ) {
            hasToParse = false;
          }
          if (hasToParse === true) {
            if (typeof entry.options.parsing.section_regex !== "undefined") {
              var section = new RegExp(
                entry.options.parsing.section_regex,
                "i"
              ).exec(line);
              if (section !== null && section.length >= 2) {
                result.children.push({ name: section[1], type: "section" });
              }
            }
            if (typeof entry.options.parsing.entry_regex !== "undefined") {
              var elem = new RegExp(
                entry.options.parsing.entry_regex,
                "i"
              ).exec(line);
              if (elem !== null && elem.length >= 4) {
                var file = null;
                var rel;
                // first see if path is already a file
                try {
                  if (elem[2].charAt(0) === "/") {
                    // absolute repo path
                    rel = elem[2].substr(1);
                  } else {
                    rel = path.join(path.dirname(entry.options.path), elem[2]);
                  }
                  if (fs.statSync(path.join(root_dir, rel)).isFile() === true) {
                    file = rel;
                  }
                } catch (err) {}
                // then try to find target file
                if (file === null) {
                  for (var i = 0; i < entry.options.target_file.length; i++) {
                    try {
                      rel = path.join(
                        path.dirname(entry.options.path),
                        elem[2],
                        entry.options.target_file[i]
                      );
                      fs.accessSync(
                        path.join(root_dir, rel),
                        fs.constants.F_OK
                      );
                      file = rel;
                      break;
                    } catch (err) {}
                  }
                }
                // only write output if we found a target
                if (file !== null) {
                  var fileEntry = {
                    name: pretty ? self.makePrettyName(elem[1]) : elem[1],
                    type: "file",
                    options: { path: file },
                  };
                  fileEntry.slug = self.createSlugFromName(fileEntry.name);
                  result.children.push(fileEntry);
                }
              }
            }
          }
        });
        return result;
      };

      this.handleListdirsEntry = function (entry) {
        var result = {
          name: entry.name,
          type: "listfiles",
          ref: entry.ref,
          children: [],
        };
        var directories = self
          .listDirectory(entry.options.path)
          .filter(function (elem) {
            return elem.stats.isDirectory() === true;
          });
        directories.forEach(function (dir) {
          var file = null;
          for (var i = 0; i < entry.options.target_file.length; i++) {
            try {
              var rel = path.join(
                entry.options.path,
                dir.name,
                entry.options.target_file[i]
              );
              fs.accessSync(path.join(root_dir, rel), fs.constants.F_OK);
              file = rel;
              break;
            } catch (err) {}
          }
          if (file !== null) {
            var fileEntry = {
              name: self.makePrettyName(dir.name),
              type: "file",
              options: { path: file },
            };
            fileEntry.slug = self.createSlugFromName(fileEntry.name);
            result.children.push(fileEntry);
          }
        });
        return result;
      };

      this.handleListfilesEntry = function (entry) {
        var result = {
          name: entry.name,
          type: "listfiles",
          ref: entry.ref,
          children: [],
        };
        var files = self
          .listDirectory(entry.options.path)
          .filter(function (elem) {
            return elem.stats.isFile() === true;
          });
        files.forEach(function (file) {
          if (entry.options.blacklist.indexOf(file.name) === -1) {
            var fileEntry = {
              name: self.makePrettyName(file.name),
              type: "file",
              options: { path: path.join(entry.options.path, file.name) },
            };
            fileEntry.slug = self.createSlugFromName(fileEntry.name);
            result.children.push(fileEntry);
          }
        });
        return result;
      };

      this.handleStaticlistEntry = function (entry) {
        var result = {
          name: entry.name,
          type: "listfiles",
          ref: entry.ref,
          children: [],
        };
        entry.children.forEach(function (child) {
          result.children.push(self.handleMenuEntry(child));
        });
        return result;
      };

      this.handleStaticfileEntry = function (entry) {
        var result = {
          name: entry.name,
          type: "file",
          options: { path: path.join(entry.options.path) },
        };
        result.slug = self.createSlugFromName(result.name);
        return result;
      };

      this.handleStaticrefEntry = function (entry) {
        var result = {
          name: entry.name,
          type: "ref",
          options: { path: entry.options.path },
        };
        return result;
      };

      this.handleSectionEntry = function (entry) {
        var result = { name: entry.name, type: "section" };
        return result;
      };

      this.handleParsefoldersEntry = function (entry) {
        var sections = [];
        var result = {
          name: entry.name,
          type: "listfiles",
          ref: entry.ref,
          children: [
            {
              name: "README",
              type: "file",
              options: {
                path: path.join(entry.options.path, entry.options.base_toc),
              },
              slug: "readme",
            },
            {
              name: "README",
              type: "buildtoc",
              options: {
                path: path.join(entry.options.path, entry.options.base_toc),
                sections: sections,
              },
            },
          ],
        };

        entry.options.folders.forEach(function (folder) {
          result.children.push({
            name: folder.title,
            type: "section",
          });

          var entries = [];
          sections.push({
            title: folder.title,
            title_level: folder.title_level,
            entries: entries,
          });

          self
            .listDirectory(path.join(entry.options.path, folder.path))
            .filter(function (elem) {
              return elem.stats.isFile() === true;
            })
            .forEach(function (elem) {
              var text = fs
                .readFileSync(
                  path.join(
                    root_dir,
                    entry.options.path,
                    folder.path,
                    elem.name
                  )
                )
                .toString();

              var titleMatch = text.match(/^# (.*)$/m);

              if (titleMatch) {
                result.children.push({
                  name: titleMatch[1],
                  type: "file",
                  options: {
                    path: path.join(entry.options.path, folder.path, elem.name),
                  },
                  slug: self.createSlugFromName(titleMatch[1]),
                });
                entries.push({
                  name: titleMatch[1],
                  path: path.join(folder.path, elem.name),
                });
              }
            });
        });

        return result;
      };

      this.listDirectory = function (relPath) {
        var result = [];
        var entries = fs.readdirSync(path.join(root_dir, relPath));
        entries.forEach(function (entry) {
          result.push({
            name: entry,
            stats: fs.statSync(path.join(root_dir, relPath, entry)),
          });
        });
        return result;
      };

      this.makePrettyName = function (name) {
        var name_pretty = name;
        if (name_pretty.indexOf(".") > -1) {
          name_pretty = name_pretty.substr(0, name_pretty.indexOf("."));
        }
        name_pretty = name_pretty.replace("-", " ");
        var words = name_pretty.split(" ");
        words = words.map(function (word) {
          return self.firstCapitalize(word);
        });
        return words.join(" ");
      };

      this.createSlugFromName = function (name) {
        return name
          .toLowerCase()
          .replace(/ /g, "-")
          .replace(/[-]+/g, "-")
          .replace(/[^\w-]+/g, "");
      };

      this.firstCapitalize = function (text) {
        return text.charAt(0).toUpperCase() + text.substr(1);
      };

      // finally, run the build!
      this.build();
    }
  );
};
