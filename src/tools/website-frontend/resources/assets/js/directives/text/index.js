"use strict";

var angular = require("angular");

angular
  .module("elektra.rest.angular")
  .directive("dateNow", require("./DateNowDirective"))
  .directive("toLowerCase", require("./InputToLowerCaseDirective"));
