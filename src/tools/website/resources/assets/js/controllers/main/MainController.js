"use strict";

module.exports = [
  "$rootScope",
  "$scope",
  "Logger",
  "$state",
  "$anchorScroll",
  "webStructure",
  "config",
  "$translate",
  function (
    $rootScope,
    $scope,
    Logger,
    $state,
    $anchorScroll,
    webStructure,
    config,
    $translate
  ) {
    $scope.$rootScope = $rootScope;
    $scope.$state = $state;

    // build the dynamic menu
    $scope.menu = webStructure;
    $rootScope.config = config;

    // build date
    $scope.builddate = {
      date: new Date(
        document
          .querySelector('meta[name="build-date"]')
          .getAttribute("content")
      ),
    };
    $scope.builddate.timezoneOffset =
      $scope.builddate.date.getTimezoneOffset() / -60;
    $scope.builddate.pretty =
      $scope.builddate.date.toLocaleString() +
      " (" +
      ($scope.builddate.timezoneOffset >= 0 ? "+" : "") +
      $scope.builddate.timezoneOffset +
      "h UTC)";

    //        var vm = this;
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

    this.scrollToTop = function () {
      $anchorScroll();
    };

    Logger.info("Main template ready");

    // init after view has been loaded (template parsed, translation happend)
    angular.element(document).ready(function () {
      docsearch({
        apiKey: "7d1e7bc1f97b53de246aaefa29484be9",
        indexName: "elektra",
        inputSelector: "#searchboxdocsearch",
        debug: false, // Set debug to true if you want to inspect the dropdown
      });
    });
  },
];
