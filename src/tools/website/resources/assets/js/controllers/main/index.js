"use strict";

var angular = require("angular");

angular
  .module("elektra.rest.angular")
  .controller("MainController", require("./MainController"));

require("./website");
