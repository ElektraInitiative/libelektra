"use strict";

var angular = require("angular");

angular
  .module("elektra.rest.angular")
  .controller("WebsiteHomeController", require("./WebsiteHomeController"))
  .controller(
    "WebsiteListfilesController",
    require("./WebsiteListfilesController")
  );
