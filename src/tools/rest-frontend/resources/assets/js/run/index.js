'use strict';

var angular = require('angular');

angular.module('elektra.rest.angular')
	   .run(require('./logger'))
	   .run(require('./states'));