(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .controller('MainController', MainController);

    MainController.$inject = [
        '$rootScope', '$scope', 'Logger', '$state'
    ];

    function MainController($rootScope, $scope, Logger, $state) {

        var vm = this;
        $scope.$rootScope = $rootScope;

//        vm.currentLanguage = 'de';

//        vm.changeLanguage = changeLanguage;
//        vm.goSearch = goSearch;

//        $rootScope.$on('$translateChangeSuccess', function() {
//            vm.currentLanguage = $translate.use();
//        });

//        function changeLanguage(newLanguage) {
//
//            $translate.use(newLanguage);
//            i18nService.setCurrentLang(newLanguage);
//
//        }

        this.goSearch = function() {

            if($rootScope.entriesSearchString) {
                Logger.log('Go to search');
                $state.go('main.entries.search');
            }

        };

        Logger.info("Main template ready");

    }

})();