(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .controller('HomeController', HomeController);

    HomeController.$inject = ['$scope', 'Logger', '$state'];

    function HomeController($scope, Logger, $state) {

        var vm = this;

        Logger.info("Home controller ready");

    }

})();