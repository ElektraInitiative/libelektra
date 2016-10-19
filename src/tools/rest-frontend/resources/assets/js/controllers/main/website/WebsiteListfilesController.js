(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .controller('WebsiteListfilesController', WebsiteListfilesController);

    WebsiteListfilesController.$inject = [
		'$scope', 'Logger', '$state', '$stateParams', 'WebsiteService', 'files'
	];

    function WebsiteListfilesController($scope, Logger, $state, $stateParams, WebsiteService, files) {

        var vm = this;

		$scope.$state = $state;
		$scope.files = files;

		function goToDefaultFile() {
			$state.go($state.current.name, {
				file: files.filter(function(elem) {
					return elem.type === 'file';
				})[0].slug
			});
		}

		if($stateParams.file === null) {
			goToDefaultFile();
		} else {
			var filtered = files.filter(function(elem) {
				return elem.slug === $stateParams.file;
			});
			if(filtered.length === 0) {
				goToDefaultFile();
			} else {
				$scope.currentFile = filtered[0];
			}
		}

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