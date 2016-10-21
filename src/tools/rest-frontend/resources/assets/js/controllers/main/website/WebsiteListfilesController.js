'use strict';

var angular = require('angular');

module.exports = function($scope, Logger, $state, $compile, $timeout, marked, files, currentFile) {

	var vm = this;

	$scope.$state = $state;
	$scope.files = files;
	$scope.currentFile = currentFile;

	var doc = marked($scope.currentFile.content);
	Logger.log(doc);
	doc = $compile(doc)($scope);
	angular.element(document.getElementById('markdown-document')).html(doc);

	Logger.info("Website listfiles controller ready");

};