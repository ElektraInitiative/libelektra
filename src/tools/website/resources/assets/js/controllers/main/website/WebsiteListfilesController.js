"use strict";

var angular = require("angular");

module.exports = [
  "$scope",
  "Logger",
  "$state",
  "$compile",
  "config",
  "marked",
  "files",
  "currentFile",
  function (
    $scope,
    Logger,
    $state,
    $compile,
    config,
    marked,
    files,
    currentFile
  ) {
    var vm = this;

    $scope.$state = $state;
    $scope.githubRoot =
      config.github.website.root + config.github.website.paths.doc_root;

    $scope.files = files;
    $scope.currentFile = currentFile;

    var doc = marked.parse($scope.currentFile.content);
    doc = $compile(doc)($scope);
    angular.element(document.getElementById("markdown-document")).html(doc);

    Logger.info("Website listfiles controller ready");
  },
];
