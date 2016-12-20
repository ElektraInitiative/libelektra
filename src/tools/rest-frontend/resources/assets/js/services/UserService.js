'use strict';

var angular = require('angular');

module.exports = function (Logger, $http, $q, config) {

    var service = this;

    this.cache = {
        search: {
            cached: false,
            users: {},
            params: {
                filter: "",
                filterby: "",
                sort: "",
                sortby: "",
                rows: 0,
                offset: 0
            }
        },
        currentUser: {
            hasUser: false,
            lastFetch: 0,
            user: {}
        }
    };


    this.clear = function () {
        service.cache.currentUser.hasCache = false;
        service.cache.currentUser.lastFetch = 0;
        service.cache.currentUser.user = {};
    };

    this.get = function (username, currentUser, force) {
        Logger.info('Attempting to load user details');

        currentUser = (typeof currentUser === 'undefined') ? false : currentUser;
        force = (typeof force === 'undefined') ? false : force;
        if (service.cache.currentUser.lastFetch + config.jwt.validity < Math.floor(Date.now() / 1000)) {
            force = true;
        }

        var deferred = $q.defer();

        if (currentUser === true) {
            if (!service.cache.currentUser.hasUser || force) {
                $http.get(config.backend.root + 'user?current=true', {
                    // custom options
                }).then(function (response) {
                    service.cache.currentUser.user = response.data;
                    service.cache.currentUser.lastFetch = Math.floor(Date.now() / 1000);
                    service.cache.currentUser.hasUser = true;
                    deferred.resolve(service.cache.currentUser.user);
                }, function (response) {
                    deferred.reject(response);
                });
            } else {
                deferred.resolve(service.cache.currentUser.user);
            }
        } else {
            $http.get(config.backend.root + 'user/' + username, {
                // custom options
            }).then(function (response) {
                deferred.resolve(response.data);
            }, function (response) {
                deferred.reject(response);
            });
        }

        return deferred.promise;
    };

    this.update = function (username, data, currentUser) {

        Logger.info('Attempting to update user');

        currentUser = (typeof currentUser === 'undefined') ? false : currentUser;

        if (currentUser === true) {
            return $http.put(config.backend.root + 'user?current=true', data, {
                // custom options
            });
        } else {
            return $http.put(config.backend.root + 'user/' + username, data, {
                // custom options
            });
        }

    };

    this.search = function (params, force) {

        Logger.info('Load users');

        var deferred = $q.defer();

        if (!service.cache.search.cached || !angular.equals(service.cache.search.params, params) || force)
        {
            Logger.info('Loading users');
            $http.get(config.backend.root + 'user', {
                params: params
            }).success(function (data) {
                service.cache.search.users = data;
                service.cache.search.params = params;
                deferred.resolve(service.cache.search.users);
            }).error(function (data) {
                deferred.reject('Error loading data');
            });
        } else {
            deferred.resolve(service.cache.search.users);
        }

        return deferred.promise;

    };

    this.hasSearchCache = function () {
        return this.cache.search.cached;
    };

    this.getSearchCache = function () {
        return this.cache.search.users;
    };

    this.getSearchFilter = function () {
        return this.cache.search.params.filter;
    };

    Logger.info('User service ready!');

};
