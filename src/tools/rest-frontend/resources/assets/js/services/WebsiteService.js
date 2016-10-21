'use strict';

module.exports = function(Logger, $http, $q, config) {

	var service = this;


	this.loadFile = function(url) {

		var deferred = $q.defer();

		$http.get(config.backend.website.content_root + url, {
			skipAuthorization: true
		}).success(function(data) {
			deferred.resolve(data);
		}).error(function(data) {
			deferred.reject(data);
		});

		return deferred.promise;

	};

	Logger.info('Website service ready!');

};