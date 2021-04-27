"use strict";

module.exports = [
  function () {
    return {
      restrict: "A",
      require: "ngModel",
      link: function (scope, element, attrs, modelCtrl) {
        var lower = function (inputValue) {
          if (inputValue === undefined) inputValue = "";
          var lowered = inputValue.toLowerCase();
          if (lowered !== inputValue) {
            modelCtrl.$setViewValue(lowered);
            modelCtrl.$render();
          }
          return lowered;
        };
        modelCtrl.$parsers.push(lower);
        lower(scope[attrs.ngModel]);
      },
    };
  },
];
