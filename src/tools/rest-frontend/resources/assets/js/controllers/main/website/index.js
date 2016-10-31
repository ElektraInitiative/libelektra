'use strict';

var angular = require('angular');

angular.module('elektra.rest.angular')
	   .controller('WebsiteHomeController', require('./WebsiteHomeController'))
	   .controller('WebsiteConversionController', require('./WebsiteConversionController'))
	   .controller('WebsiteListfilesController', require('./WebsiteListfilesController'));