'use strict';

module.exports = function($rootScope, $scope, Logger, $state, $anchorScroll, webStructure) {

	var vm = this;
	$scope.$rootScope = $rootScope;

	// build the dynamic menu
	$scope.menu = webStructure;

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

	this.scrollToTop = function() {
		$anchorScroll();
	};

	Logger.info("Main template ready");

};