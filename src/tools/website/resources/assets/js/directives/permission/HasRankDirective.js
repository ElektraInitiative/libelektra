"use strict";

module.exports = [
  "Logger",
  "$rootScope",
  function(Logger, $rootScope) {
    return {
      restrict: "A",
      scope: { hasRank: "=" },
      link: function($scope, elem, attrs) {
        $rootScope.$watch("currentUser", function() {
          Logger.info("Checking for rank: " + $scope.hasRank);
          if (
            $rootScope.currentUser &&
            $rootScope.currentUser.rank >= $scope.hasRank
          ) {
            elem.show();
          } else {
            elem.hide();
          }
        });
      }
    };
  }
];
