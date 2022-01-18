"use strict";

// external dependencies
require("jquery");
global.jQuery = require("jquery");
global.$ = global.jQuery;

// angular itself
var angular = require("angular");

// angular dependencies
require("@iamadamjowett/angular-logger-max/logger.service");
require("angular-animate");
require("angular-breadcrumb");
require("angular-clipboard");
require("angular-file-saver");
require("angular-marked");
require("angular-messages");
require("angular-sanitize");
require("angular-slugify");
require("angular-translate");
require("angular-translate-loader-static-files");
require("angular-typewriter");
require("angular-ui-bootstrap");
require("angular-ui-notification");
require("@uirouter/angularjs");
require("ng-tags-input");
require("satellizer");

// the angular module
angular.module("elektra.rest.angular", [
  "angular-clipboard",
  "angular-logger-max",
  require("angular-typewriter"),
  "hc.marked",
  "ncy-angular-breadcrumb",
  "ngAnimate",
  "ngFileSaver",
  "ngMessages",
  "ngSanitize",
  "ngTagsInput",
  "pascalprecht.translate",
  "satellizer",
  "slugifier",
  "ui.bootstrap",
  "ui.router",
  "ui-notification",
]);

// application includes
require("./config");
require("./controllers");
require("./directives");
require("./filter");
require("./run");
require("./services");
