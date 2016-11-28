'use strict';

var angular = require('angular');

module.exports = function (Logger, $http, $q, config) {

    var service = this;

    this.cache = {
        formats: [],
        typeaheads: {
            cached: false,
            data: {
                organizations: [],
                applications: [],
                scopes: [],
                tags: []
            }
        },
        search: {
            cached: false,
            entries: {},
            params: {
                filter: "",
                filterby: "",
                sort: "",
                sortby: "",
                rows: 0,
                offset: 0
            }
        }
    };


    this.create = function (entry) {

        Logger.info('Attempting to create entry.');

        return $http.post(config.backend.root + 'database', entry, {
            // custom options
        });

    };

    this.update = function (key, entry) {

        Logger.info('Attempting to update entry.');

        return $http.put(config.backend.root + 'database/' + key, entry, {
            // custom options
        });

    };

    this.delete = function (key) {

        Logger.info('Attempting to delete entry.');

        return $http.delete(config.backend.root + 'database/' + key, {
            // custom options
        });

    };

    this.get = function (key) {

        Logger.info('Attempting to download entry: ' + key);

        var deferred = $q.defer();

        if (key.length <= 1) {
            deferred.reject('Invalid key');
        } else {
            var url = config.backend.root + 'database/' + key;
            $http.get(url).success(function (data) {
                deferred.resolve(data);
            }).error(function (data) {
                deferred.reject('Error loading data');
            });
        }

        return deferred.promise;

    };

    this.search = function (params, force) {

        Logger.info('Load entries');

        var deferred = $q.defer();

        if (!service.cache.search.cached || !angular.equals(service.cache.search.params, params) || force)
        {
            Logger.info('Loading entries');
            $http.get(config.backend.root + 'database', {
                params: params
            }).success(function (data) {
                service.cache.search.entries = data;
                service.cache.search.params = params;
                deferred.resolve(service.cache.search.entries);
            }).error(function (data) {
                deferred.reject('Error loading data');
            });
        } else {
            deferred.resolve(service.cache.search.entries);
        }

        return deferred.promise;

    };

    this.hasSearchCache = function () {
        return this.cache.search.cached;
    };

    this.getSearchCache = function () {
        return this.cache.search.entries;
    };

    this.getSearchFilter = function () {
        return this.cache.search.params.filter;
    };


    this.loadAvailableFormats = function () {

        Logger.info('Attempting to load available formats.');

        var deferred = $q.defer();

        if (service.cache.formats.length > 0) {
            deferred.resolve(service.cache.formats);
        } else {
            $http.get(config.backend.root + 'conversion/formats', {
                // custom options
            }).success(function (data) {
                service.cache.formats = data;
                deferred.resolve(service.cache.formats);
            }).error(function (data) {
                deferred.reject(data);
            });
        }

        return deferred.promise;

    };

    this.loadAvailableTypeaheads = function () {

        Logger.info('Attempting to load available typeaheads.');

        var deferred = $q.defer();

        if (service.cache.typeaheads.cache === true) {
            deferred.resolve(service.cache.typeaheads.data);
        } else {
            $http.get(config.backend.root + 'database', {
                // custom options
            }).success(function (data) {
                if (data.elements > 0 && typeof data.entries !== 'undefined') {
                    data.entries.forEach(function (entry) {
                        if (service.cache.typeaheads.data.organizations.indexOf(entry.key.organization) === -1) {
                            service.cache.typeaheads.data.organizations.push(entry.key.organization);
                        }
                        if (service.cache.typeaheads.data.applications.indexOf(entry.key.application) === -1) {
                            service.cache.typeaheads.data.applications.push(entry.key.application);
                        }
                        if (service.cache.typeaheads.data.scopes.indexOf(entry.key.scope) === -1) {
                            service.cache.typeaheads.data.scopes.push(entry.key.scope);
                        }
                        entry.tags.forEach(function (tag) {
                            if (service.cache.typeaheads.data.tags.indexOf(tag) === -1) {
                                service.cache.typeaheads.data.tags.push(tag);
                            }
                        });
                    });
                }
                service.cache.typeaheads.cache = true;
                deferred.resolve(service.cache.typeaheads.data);
            }).error(function (data) {
                deferred.reject(data);
            });
        }

        return deferred.promise;

    };

};
