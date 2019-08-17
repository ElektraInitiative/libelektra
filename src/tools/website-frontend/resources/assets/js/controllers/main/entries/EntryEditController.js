'use strict';

var angular = require('angular');

module.exports = function ($scope, Logger, $state, EntryService, Notification, Slug, formats, entry, typeaheads) {

    var vm = this;

    Logger.log(entry);
    $scope.formats = formats.map(function(elem) {
        var name = elem.plugin.name;
        var space = name.indexOf(' ');
        if(space > -1) {
            name = name.substring(0, space);
        }
        elem.plugin.nameWithoutConf = name;
        return elem;
    });
    $scope.entry = {
        title: entry.meta.title,
        description: entry.meta.description,
        tags: (!!entry.meta.tags) ? entry.meta.tags.map(function (elem) {
            return {text: elem};
        }) : [],
        configuration: {
            format: $scope.formats.filter(function (elem) {
                return elem.plugin.name === entry.value[0].plugin;
            })[0],
			formatconf: '',
            value: entry.value[0].value
        }
    };
    $scope.typeaheads = typeaheads;

    this.submit = function () {
        Logger.info('Attempting to update entry.');

        var tmp = {};
        angular.copy($scope.entry, tmp);
        tmp.tags = $scope.entry.tags.map(function (elem) {
            return elem.text;
        });
        tmp.configuration.format = $scope.entry.configuration.format.plugin.name;
		
		if (tmp.configuration.formatconf !== '') {
			tmp.configuration.format += ' ' + tmp.configuration.formatconf;
		}
		delete tmp.configuration.formatconf;

        EntryService.update(entry.key.full, tmp).then(function (response) {
            Logger.info('Update entry result: ' + JSON.stringify(response.data));
            Notification.success({
                title: 'APP.ENTRIES.EDIT.NOTIFICATION.HEADER',
                message: 'APP.ENTRIES.EDIT.NOTIFICATION.MESSAGE.' + response.data.i18n
            });

            $state.go('main.entries.details', {
                entry: entry.key.full
            });
        }, function (response) {
            Notification.error({
                title: 'APP.ENTRIES.EDIT.NOTIFICATION.HEADER',
                message: 'APP.ENTRIES.EDIT.NOTIFICATION.MESSAGE.' + response.data.i18n
            });
        });
    };

    this.abort = function () {
        $state.go('main.entries.details', {
            entry: entry.key.full
        });
    };

    Logger.info('Edit entry controller ready');

};
