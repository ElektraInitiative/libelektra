(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .controller('EntryDeleteConfirmationController', EntryDeleteConfirmationController);

    EntryDeleteConfirmationController.$inject = [
        '$uibModalInstance', '$scope', 'Logger', 'entry'
	];

    function EntryDeleteConfirmationController($uibModalInstance, $scope, Logger, entry) {

        var vm = this;

		$scope.entry = entry;

		this.ok = function() {
			$uibModalInstance.close(true);
		};

		this.abort = function() {
			$uibModalInstance.dismiss(false);
		};

        Logger.info('Entry delete confirmation controller ready');

    }

})();