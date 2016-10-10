(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .controller('EntryDetailsController', EntryDetailsController);

    EntryDetailsController.$inject = [
        '$scope', 'Logger', '$state', 'FileSaver', 'Blob', 'entry', 'Notification', 'EntryService', '$uibModal'
    ];

    function EntryDetailsController($scope, Logger, $state, FileSaver, Blob, entry, Notification, EntryService,
			$uibModal) {

        var vm = this;

        $scope.entry = entry;
		$scope.clipboardSupported = false;

        this.saveConvertedConfiguration = function(config) {
            Logger.info('Configuration:' + config);
            var blob = new Blob([config.value], { type:'text/plain;charset=utf-8;' });
            FileSaver.saveAs(blob, $scope.entry.key.slug + '.' + config.format, true);
        };

		this.copyClipboardSuccess = function() {
			Notification.success({
				title: 'APP.ENTRIES.DETAILS.NOTIFICATION.CLIPBOARD.HEADER',
				message: 'APP.ENTRIES.DETAILS.NOTIFICATION.CLIPBOARD.MESSAGE.SUCCESS'
			});
		};

		this.copyClipboardFailed = function() {
			Notification.error({
				title: 'APP.ENTRIES.DETAILS.NOTIFICATION.CLIPBOARD.HEADER',
				message: 'APP.ENTRIES.DETAILS.NOTIFICATION.CLIPBOARD.MESSAGE.ERROR'
			});
		};

		this.deleteEntry = function() {
			var confirmation = $uibModal.open({
				templateUrl: 'pages/main/entries/delete_confirmation.html',
				controller: 'EntryDeleteConfirmationController as ctrl',
				resolve: {
					entry: function() {
						return entry;
					}
				}
			});

			confirmation.result.then(function(result) {
				if(result === true) {
					EntryService.delete(entry.key.full).then(function(response) {
						Notification.success({
							title: 'APP.ENTRIES.DELETE.NOTIFICATION.HEADER',
							message: 'APP.ENTRIES.DELETE.NOTIFICATION.MESSAGE.' + response.data.i18n
						});
						$state.go('main.entries.search');
					}, function(response) {
						Notification.error({
							title: 'APP.ENTRIES.DELETE.NOTIFICATION.HEADER',
							message: 'APP.ENTRIES.DELETE.NOTIFICATION.MESSAGE.' + response.data.i18n
						});
					});
				}
			});
		};

        Logger.info("Entry details ready");

    }

})();