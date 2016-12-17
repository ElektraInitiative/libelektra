'use strict';

module.exports = function ($rootScope, $scope, Logger, $state, $anchorScroll, webStructure, config) {

    var vm = this;
    $scope.$rootScope = $rootScope;
    $scope.$state = $state;

    // build the dynamic menu
    $scope.menu = webStructure;
    $scope.config = config;

    // build date
    $scope.builddate = {
        date: new Date(document.querySelector('meta[name="build-date"]').getAttribute('content'))
    };
    $scope.builddate.timezoneOffset = $scope.builddate.date.getTimezoneOffset() / -60;
    $scope.builddate.pretty = $scope.builddate.date.toLocaleString() + ' (' +
            (($scope.builddate.timezoneOffset >= 0) ? '+' : '') + $scope.builddate.timezoneOffset + 'h UTC)';

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

    this.goSearch = function () {

        if ($rootScope.entriesSearchString) {
            Logger.log('Go to search');
            $state.go('main.entries.search');
        }

    };

    this.scrollToTop = function () {
        $anchorScroll();
    };

    Logger.info("Main template ready");

};
