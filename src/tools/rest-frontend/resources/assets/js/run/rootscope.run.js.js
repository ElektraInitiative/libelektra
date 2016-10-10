(function() {

    'use strict';

    angular.module('elektra.rest.angular').run([
        'Logger',
		'$rootScope',
		'$state',
		'$stateParams',
        function(Logger, $rootScope, $state, $stateParams) {

			$rootScope.$state = $state;
			$rootScope.$stateParams = $stateParams;

        }
    ]);

})();