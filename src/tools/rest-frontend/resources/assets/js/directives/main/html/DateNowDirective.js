(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .directive('dateNow', DateNow);

    DateNow.$inject = [
		'$filter'
	];

    function DateNow($filter) {

        return function($scope ,element, attrs) {
			element.text($filter('date')(new Date(), attrs.dateNow));
		};

    }

})();