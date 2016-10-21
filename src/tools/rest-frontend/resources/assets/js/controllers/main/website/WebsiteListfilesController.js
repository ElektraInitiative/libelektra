'use strict';

module.exports = function($scope, Logger, $state, files, currentFile) {

	var vm = this;

	$scope.$state = $state;
	$scope.files = files;
	$scope.currentFile = currentFile;

	Logger.info("Website listfiles controller ready");

};