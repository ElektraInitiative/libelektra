(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .filter('firstCapitalize', FirstCapitalizeFilter);

    FirstCapitalizeFilter.$inject = [];

    function FirstCapitalizeFilter() {

        return function(input, eachWord) {
			eachWord = typeof eachWord !== 'undefined' ? eachWord : false;
			if(eachWord === true) {
				return input.split(' ').map(function(elem) {
					return elem.charAt(0).toUpperCase() + elem.substr(1).toLowerCase();
				}).join(' ');
			} else {
				return (!!input) ? input.charAt(0).toUpperCase() + input.substr(1).toLowerCase() : '';
			}
		};

    }

})();