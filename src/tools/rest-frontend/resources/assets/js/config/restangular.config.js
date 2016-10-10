(function() {

    'use strict';

    angular.module('elektra.rest.angular').config([
        'RestangularProvider',
        function(RestangularProvider) {

            RestangularProvider.setBaseUrl('http://localhost:8080');

        }
    ]);

})();