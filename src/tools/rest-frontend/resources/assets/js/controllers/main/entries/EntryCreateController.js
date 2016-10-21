'use strict';

var angular = require('angular');

module.exports = function($scope, Logger, EntryService, $state, Notification, formats) {

	var vm = this;

	$scope.isCreate = true;

	$scope.entry = {
		tags: [],
		configuration: {
			format: {}
		}
	};
	$scope.formats = formats;
	$scope.entry.configuration.format = $scope.formats[0];

	this.submit = function() {
		Logger.info('Attempting to create new entry.');

		var tmp = {};
		angular.copy($scope.entry, tmp);
		tmp.tags = $scope.entry.tags.map(function(elem){
			return elem.text;
		});
		tmp.configuration.format = $scope.entry.configuration.format.plugin.name;

		EntryService.create(tmp).then(function(response) {
			Logger.info('Create entry result: ' + JSON.stringify(response.data));
			Notification.success({
				title: 'APP.ENTRIES.CREATE.NOTIFICATION.HEADER',
				message: 'APP.ENTRIES.CREATE.NOTIFICATION.MESSAGE.' + response.data.i18n
			});

			$state.go('main.entries.details', {
				entry: tmp.organization + '/' + tmp.application + '/' + tmp.scope + '/' + tmp.slug
			});
		}, function(response) {
			Notification.error({
				title: 'APP.ENTRIES.CREATE.NOTIFICATION.HEADER',
				message: 'APP.ENTRIES.CREATE.NOTIFICATION.MESSAGE.' + response.data.i18n
			});
		});
	};

	Logger.info('New entry controller ready');

};