"use strict";

module.exports = [
  "$parse",
  function ($parse) {
    return {
      restrict: "A",
      require: "?ngModel",
      link: function ($scope, elem, attrs, ngModel) {
        if (!ngModel) return; // do nothing if no ng-model

        // watch own value and re-validate on change
        $scope.$watch(attrs.ngModel, function () {
          validate();
        });

        // observe the other value and re-validate on change
        $scope.$watch(attrs.equalInput, function () {
          validate();
        });

        var validate = function () {
          // values
          var val1 = ngModel.$viewValue;
          var val2 = $parse(attrs.equalInput)($scope);

          // set validity
          ngModel.$setValidity("equalInput", val1 === val2);
        };
      },
    };
  },
];
