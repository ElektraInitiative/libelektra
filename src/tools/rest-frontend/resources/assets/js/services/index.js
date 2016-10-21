'use strict';

var angular = require('angular');

angular.module('elektra.rest.angular')
	   .service('ConversionService', require('./ConversionService'))
	   .service('EntryService', require('./EntryService'))
	   .service('ReportService', require('./ReportService'))
	   .service('UserService', require('./UserService'))
	   .service('WebsiteService', require('./WebsiteService'));