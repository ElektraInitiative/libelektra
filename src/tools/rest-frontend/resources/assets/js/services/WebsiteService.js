(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .service('WebsiteService', WebsiteService);

    WebsiteService.$inject = [
        'Logger', '$http', '$q', 'config'
    ];

    function WebsiteService(Logger, $http, $q, config) {

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

    }

})();