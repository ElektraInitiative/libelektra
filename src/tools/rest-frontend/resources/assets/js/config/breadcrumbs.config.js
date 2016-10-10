(function() {

    'use strict';

    angular.module('elektra.rest.angular').config([
        '$breadcrumbProvider',
        function($breadcrumbProvider) {

            // configure breadcrumbs
            $breadcrumbProvider.setOptions({
                templateUrl: 'includes/breadcrumbs.html',
                prefixStateName: 'main'
            });

        }
    ]);

})();