'use strict';

var angular = require('angular');

angular.module('elektra.rest.angular', [
	'ab-base64',
	'angular-clipboard',
	'angular-logger-max',
	'hc.marked',
	'ncy-angular-breadcrumb',
	'ngAnimate',
	'ngFileSaver',
	'ngMessages',
	'ngSanitize',
	'ngTagsInput',
	'pascalprecht.translate',
	'satellizer',
	'ui.bootstrap',
	'ui.router',
	'ui-notification'
]);

// one require per directory
require('./config');
require('./controllers');
require('./directives');
require('./filter');
require('./run');
require('./services');