'use strict';

var angular = require('angular');

module.exports = function ($scope, Logger, $state, EntryService, Notification, Slug, config, formats, typeaheads) {

    var vm = this;

    $scope.isCreate = true;

    $scope.cb = {
        createScopeManually: false
    };
    $scope.entry = {
        organization: config.website.defaults.entry.form.organization,
        scope: config.website.defaults.entry.form.scope,
        tags: [],
        configuration: {
            format: {},
            formatconf: '',
            value: ''
        }
    };
    $scope.formats = formats.map(function(elem) {
        var name = elem.plugin.name;
        var space = name.indexOf(' ');
        if(space > -1) {
            name = name.substring(0, space);
        }
        elem.plugin.nameWithoutConf = name;
        return elem;
    });
    $scope.entry.configuration.format = $scope.formats[0];
    $scope.typeaheads = typeaheads;

    $scope.$watch('entry.title', function () {
        if ($scope.cb.createScopeManually === true) {
            return;
        }
        $scope.entry.slug = Slug.slugify($scope.entry.title);
    }, true);

    $scope.$watch('cb.createScopeManually', function () {
        if ($scope.cb.createScopeManually === false) {
            $scope.entry.slug = Slug.slugify($scope.entry.title);
        }
    }, true);

    this.submit = function () {
        Logger.info('Attempting to create new entry.');

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

        EntryService.create(tmp).then(function (response) {
            Logger.info('Create entry result: ' + JSON.stringify(response.data));
            Notification.success({
                title: 'APP.ENTRIES.CREATE.NOTIFICATION.HEADER',
                message: 'APP.ENTRIES.CREATE.NOTIFICATION.MESSAGE.' + response.data.i18n
            });

            $state.go('main.entries.details', {
                entry: tmp.organization + '/' + tmp.application + '/' + tmp.scope + '/' + tmp.slug
            });
        }, function (response) {
            Notification.error({
                title: 'APP.ENTRIES.CREATE.NOTIFICATION.HEADER',
                message: 'APP.ENTRIES.CREATE.NOTIFICATION.MESSAGE.' + response.data.i18n
            });
        });
    };

    Logger.info('New entry controller ready');

};
