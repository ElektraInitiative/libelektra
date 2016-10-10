(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .service('UserService', UserService);

    UserService.$inject = [
        '$rootScope', 'Logger', '$http', '$q', 'config.rest.basepath'
    ];

    function UserService($rootScope, Logger, $http, $q, configRestBasepath) {

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
				user: {}
			}
		};


        this.clear = function() {
			service.cache.currentUser.hasCache = false;
            service.cache.currentUser.user = {};
        };

        this.get = function(username, currentUser, force) {
            Logger.info('Attempting to load user details');

			currentUser = (typeof currentUser === 'undefined') ? false : currentUser;
			force = (typeof force === 'undefined') ? false : force;

			var deferred = $q.defer();

			if(currentUser === true) {
				if(!service.cache.currentUser.hasUser || force) {
					$http.get(configRestBasepath + 'user?current=true', {
						// custom options
					}).then(function (response) {
						service.cache.currentUser.user = response.data;
						service.cache.currentUser.hasUser = true;
						deferred.resolve(service.cache.currentUser.user);
					}, function (response) {
						deferred.reject(response);
					});
				} else {
					deferred.resolve(service.cache.currentUser.user);
				}
			} else {
				$http.get(configRestBasepath + 'user/' + username, {
					// custom options
				}).then(function(response) {
					deferred.resolve(response.data);
				}, function(response) {
					deferred.reject(response);
				});
			}

			return deferred.promise;
		};

		this.update = function(username, data, currentUser) {

			Logger.info('Attempting to update user');

			currentUser = (typeof currentUser === 'undefined') ? false : currentUser;

			if(currentUser === true) {
				return $http.put(configRestBasepath + 'user?current=true', data, {
					// custom options
				});
			} else {
				return $http.put(configRestBasepath + 'user/' + username, data, {
					// custom options
				});
			}

		};

		this.search = function(params, force) {

            Logger.info('Load users');

            var deferred = $q.defer();

            if(!service.cache.search.cached || !angular.equals(service.cache.search.params, params) || force)
            {
                Logger.info('Loading users');
                $http.get(configRestBasepath + 'user', {
					params: params
				}).success(function(data) {
                    service.cache.search.users = data;
                    service.cache.search.params = params;
                    deferred.resolve(service.cache.search.users);
                }).error(function(data) {
                    deferred.reject('Error loading data');
                });
            } else {
                deferred.resolve(service.cache.search.users);
            }

            return deferred.promise;

        };

		this.hasSearchCache = function() {
			return this.cache.search.cached;
		};

        this.getSearchCache = function() {
            return this.cache.search.users;
        };

        this.getSearchFilter = function() {
            return this.cache.search.params.filter;
        };

    }

})();