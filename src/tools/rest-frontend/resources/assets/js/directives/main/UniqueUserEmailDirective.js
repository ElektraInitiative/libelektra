//(function() {
//
//    'use strict';
//
//    angular.module('elektra.rest.angular')
//        .directive('uniqueUserEmail', UniqueUserEmailDirective);
//
//    UniqueUserEmailDirective.$inject = ['Logger', 'Restangular', '$parse'];
//
//    function UniqueUserEmailDirective(Logger, Restangular, $parse) {
//
//        return {
//
//            restrict: 'A',
//            require: 'ngModel',
//            link: function($scope, ele, attrs, modelCtrl) {
//
//                $scope.$watch(attrs.ngModel, function() {
//
//                    Logger.log($scope);
//
//                    if(ele.val()) {
//
//                        Restangular.one('admin/users/exists/', ele.val()).get().then(function(data) {
//
//                            if(data.exists === true && $parse(attrs.uniqueUserEmail)($scope)) {
//
//                                Restangular.one('admin/users', $parse(attrs.uniqueUserEmail)($scope)).get().then(function(data) {
//
//                                    if(data.email === ele.val()) {
//
//                                        modelCtrl.$setValidity('unique', true);
//
//                                    } else {
//
//                                        modelCtrl.$setValidity('unique', false);
//
//                                    }
//
//                                });
//
//                            } else {
//
//                                modelCtrl.$setValidity('unique', !data.exists);
//
//                            }
//
//
//                        });
//                    }
//
//                });
//
//            }
//
//        }
//
//    }
//
//})();