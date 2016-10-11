(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .service('ConversionService', ConversionService);

    ConversionService.$inject = [
        'Logger', '$http', '$q', 'config'
    ];

    function ConversionService(Logger, $http, $q, config) {

        var service = this;

		this.convert = function(parameters) {

			return $http.post(config.backend.root + 'conversion', parameters, {
				// custom options
			});

		};

		Logger.info('Conversion service ready!');

    }

})();