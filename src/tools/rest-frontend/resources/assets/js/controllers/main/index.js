'use strict';

var angular = require('angular');

angular.module('elektra.rest.angular')
	   .controller('ConversionController', require('./ConversionController'))
	   .controller('HomeController', require('./HomeController'))
	   .controller('MainController', require('./MainController'));

require('./auth');
require('./entries');
require('./users');
require('./website');