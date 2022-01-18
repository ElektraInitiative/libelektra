"use strict";

module.exports = [
  "$breadcrumbProvider",
  function ($breadcrumbProvider) {
    // configure breadcrumbs
    $breadcrumbProvider.setOptions({
      templateUrl: "templates/breadcrumbs.html",
      prefixStateName: "main",
    });
  },
];
