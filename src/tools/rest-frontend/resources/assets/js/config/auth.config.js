(function() {

    'use strict';

    angular.module('elektra.rest.angular').config([
        '$authProvider',
        '$httpProvider',
        '$provide',
        'config',
        function($authProvider, $httpProvider, $provide, config) {

            $authProvider.baseUrl = config.backend.root;
            $authProvider.loginUrl = '/auth';

            // Setup interceptor that logs user out if being idle too long
            $provide.factory('redirectWhenLoggedOut', [
                '$q',
                '$injector',
                function($q, $injector) {

                    return {

                        responseError: function(rejection) {

                            // Need to use $injector.get to bring in $state or else we get
                            // a circular dependency error
                            var $state = $injector.get('$state');

                            var rejectionReasons = [401, 403, 405];

                            angular.forEach(rejectionReasons, function(value, key) {

                                if(rejection.status === value) {
                                    // Send the user to the auth state so they can login
                                    $state.go('main.auth.login');
                                }
                            });

                            return $q.reject(rejection);
                        }
                    };

                }
            ]);
            $httpProvider.interceptors.push('redirectWhenLoggedOut');

        }
    ]);

})();