"use strict";

var angular = require("angular");

angular
  .module("elektra.rest.angular")
  .service("ReportService", require("./ReportService"))
  .service("WebsiteService", require("./WebsiteService"));
