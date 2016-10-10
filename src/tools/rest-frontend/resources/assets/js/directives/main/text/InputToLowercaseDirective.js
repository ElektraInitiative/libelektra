(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .directive('toLowercase', InputToLowercaseDirective);

    InputToLowercaseDirective.$inject = ['Logger'];

    function InputToLowercaseDirective(Logger) {

        return {

            restrict: 'A',
            require: 'ngModel',
            link: function(scope, element, attrs, modelCtrl) {
                var lower = function(inputValue) {
                    if(inputValue === undefined) inputValue = '';
                    var lowered = inputValue.toLowerCase();
                    if(lowered !== inputValue) {
                        modelCtrl.$setViewValue(lowered);
                        modelCtrl.$render();
                    }
                    return lowered;
                };
                modelCtrl.$parsers.push(lower);
                lower(scope[attrs.ngModel]);
            }

        };

    }

})();