(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .controller('HomeController', HomeController);

    HomeController.$inject = [
		'$scope', 'Logger'
	];

    function HomeController($scope, Logger) {

        var vm = this;

        Logger.info("Home controller ready");

    }

})();