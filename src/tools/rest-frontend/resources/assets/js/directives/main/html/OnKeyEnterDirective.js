(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .directive('onKeyEnter', OnKeyEnter);

    OnKeyEnter.$inject = [];

    function OnKeyEnter() {

        return function ($scope, element, attrs) {

            element.bind("keydown keypress", function (event) {
                if(event.which === 13) {
                    $scope.$apply(function (){
                        $scope.$eval(attrs.onKeyEnter);
                    });

                    event.preventDefault();
                }
            });

        };

    }

})();