'use strict';

var angular = require('angular');

angular.module('elektra.rest.angular')
	   .directive('hasRank', require('./HasRankDirective'))
	   .directive('hasRankOrIsOwner', require('./HasRankOrIsOwnerDirective'));