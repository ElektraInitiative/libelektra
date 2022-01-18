"use strict";

const hljs = require("highlight.js");

module.exports = [
  "markedProvider",
  "config",
  "webStructure",
  function (markedProvider, config, webStructure) {
    markedProvider.setOptions({
      gfm: true,
      tables: true,
      highlight: function (code, lang) {
        var ret;
        try {
          if (lang) {
            ret = hljs.highlight(code, {
              language: lang,
              ignoreIllegals: true,
            }).value;
          } else {
            ret = hljs.highlightAuto(code).value;
          }
        } catch (err) {
          ret = code;
        }
        return ret;
      },
    });

    markedProvider.setRenderer({
      table: function (head, body) {
        return (
          '<table class="table table-striped table-bordered">' +
          "<thead>" +
          head +
          "</thead>" +
          "<tbody>" +
          body +
          "</tbody>" +
          "</table>"
        );
      },
      link: function (href, title, text) {
        // external link
        if (href.indexOf("://") > -1) {
          return (
            '<a href="' +
            href +
            '"' +
            (title ? ' title="' + title + '"' : "") +
            ' target="_blank">' +
            text +
            ' <i class="fa fa-external-link"></i></a>'
          );
        }
        // anchor link
        if (href.indexOf("#") > -1) {
          return (
            '<a href="' +
            href +
            '"' +
            (title ? ' title="' + title + '"' : "") +
            " >" +
            text +
            ' <i class="fa fa-external-link"></i></a>'
          );
        }
        // internal link
        var file = findFileInWebstructure(webStructure, href);
        // we don't have the file on the website, make external link
        if (file === null) {
          return (
            '<a href="' +
            config.github.website.root +
            config.github.website.paths.doc_root +
            href +
            '"' +
            (title ? ' title="' + title + '"' : "") +
            ' target="_blank">' +
            text +
            ' <i class="fa fa-external-link"></i></a>'
          );
        }
        // we have this file on the website, make internal link
        else {
          return (
            '<a ui-sref="main.dyn.' +
            file.ref +
            "({file:'" +
            file.slug +
            "'})\"" +
            (title ? ' title="' + title + '"' : "") +
            ">" +
            text +
            "</a>"
          );
        }
      },
      image: function (href, title, text) {
        // external image
        if (href.indexOf("://") > -1) {
          return (
            '<img src="' +
            href +
            '"' +
            (text ? ' alt="' + text + '"' : "") +
            (title ? ' title="' + title + '"' : "") +
            "/>"
          );
        }
        // internal link, load from github
        return (
          '<img src="' +
          config.website.content_root +
          href +
          '"' +
          (text ? ' alt="' + text + '"' : "") +
          (title ? ' title="' + title + '"' : "") +
          "/>"
        );
      },
    });

    function findFileInWebstructure(list, path) {
      var result = null;
      for (var i = 0; i < list.length; i++) {
        if (list[i].type === "submenu" || list[i].type === "listfiles") {
          result = findFileInWebstructure(list[i].children, path);
          if (result !== null && result.type === "file") {
            if (typeof result.ref === "undefined") {
              result.ref = list[i].ref;
            }
            return result;
          }
        } else if (list[i].type === "file" && list[i].options.path === path) {
          return list[i];
        }
      }
      return null; // did not find path
    }
  },
];
