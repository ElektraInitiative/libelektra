(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .controller('EntryEditController', EntryEditController);

    EntryEditController.$inject = [
        '$scope', 'Logger', 'EntryService', '$state', 'Notification', 'formats', 'entry'
    ];

    function EntryEditController($scope, Logger, EntryService, $state, Notification, formats, entry) {

        var vm = this;

		Logger.log(entry);
		$scope.formats = formats;
        $scope.entry = {
			title: entry.meta.title,
			description: entry.meta.description,
			tags: (!!entry.meta.tags) ? entry.meta.tags.map(function(elem) {
				return {text: elem};
			}) : [],
			configuration: {
				format: $scope.formats.filter(function(elem) {
					return elem.plugin === entry.value[0].plugin;
				})[0],
				value: entry.value[0].value
			}
		};

        this.submit = function() {
            Logger.info('Attempting to update entry.');

            var tmp = {};
            angular.copy($scope.entry, tmp);
            tmp.tags = $scope.entry.tags.map(function(elem){
                return elem.text;
            });
			tmp.configuration.format = $scope.entry.configuration.format.plugin;

            EntryService.update(entry.key.full, tmp).then(function(response) {
                Logger.info('Update entry result: ' + JSON.stringify(response.data));
                Notification.success({
                    title: 'APP.ENTRIES.EDIT.NOTIFICATION.HEADER',
                    message: 'APP.ENTRIES.EDIT.NOTIFICATION.MESSAGE.' + response.data.i18n
                });

                $state.go('main.entries.details', {
					entry: entry.key.full
				});
            }, function(response) {
                Notification.error({
                    title: 'APP.ENTRIES.EDIT.NOTIFICATION.HEADER',
                    message: 'APP.ENTRIES.EDIT.NOTIFICATION.MESSAGE.' + response.data.i18n
                });
            });
        };

		this.abort = function() {
			$state.go('main.entries.details', {
				entry: entry.key.full
			});
		};

        Logger.info('Edit entry controller ready');

    }

})();