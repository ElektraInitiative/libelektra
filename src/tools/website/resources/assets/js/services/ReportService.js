"use strict";

module.exports = [
  "Logger",
  "$window",
  "config",
  function (Logger, $window, config) {
    var service = this;

    this.reportIssue = function (title, message, labels) {
      var url =
        config.github.website.root +
        config.github.website.paths.issues +
        "?title=" +
        encodeURIComponent(title) +
        "&body=" +
        encodeURIComponent(message);
      labels.forEach(function (elem) {
        url += "&labels[]=" + encodeURIComponent(elem);
      });

      $window.open(url, "_blank");
    };

    Logger.info("Report service ready!");
  },
];
