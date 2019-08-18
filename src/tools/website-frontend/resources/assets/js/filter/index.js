"use strict";

var angular = require("angular");

angular
  .module("elektra.rest.angular")
  .filter("firstCapitalize", require("./FirstCapitalizeFilter"));
