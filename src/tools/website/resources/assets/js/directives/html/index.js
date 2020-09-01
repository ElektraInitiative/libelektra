"use strict";

var angular = require("angular");

angular
  .module("elektra.rest.angular")
  .directive("equalInput", require("./EqualInputDirective"))
  .directive("onKeyEnter", require("./OnKeyEnterDirective"));
