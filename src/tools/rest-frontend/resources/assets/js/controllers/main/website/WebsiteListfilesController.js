(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .controller('WebsiteListfilesController', WebsiteListfilesController);

    WebsiteListfilesController.$inject = [
		'$scope', 'Logger', 'WebsiteService', 'files'
	];

    function WebsiteListfilesController($scope, Logger, WebsiteService, files) {

        var vm = this;

		$scope.files = files;
		$scope.currentFile = files.filter(function(elem) {
			return elem.type === 'file';
		})[0];

		this.setCurrentFile = function(file) {
			$scope.currentFile = file;
			vm.loadFile();
		};

		this.loadFile = function() {
			WebsiteService.loadFile($scope.currentFile.options.path).then(function(data) {
				$scope.currentFile.content = data;
			}, function(error) {
				Logger.error('Could not load specific file');
			});
		};

		this.loadFile();
		Logger.info("Website listfiles controller ready");

    }

})();