(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .service('ConversionService', ConversionService);

    ConversionService.$inject = [
        'Logger', '$http', '$q', 'config.rest.basepath'
    ];

    function ConversionService(Logger, $http, $q, configRestBasepath) {

        var service = this;

		this.convert = function(parameters) {

			return $http.post(configRestBasepath + 'conversion', parameters, {
				// custom options
			});

		};

    }

})();