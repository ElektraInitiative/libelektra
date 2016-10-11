(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .directive('hasRankOrIsOwner', HasRankOrIsOwnerDirective);

    HasRankOrIsOwnerDirective.$inject = [
		'Logger', '$rootScope', '$parse'
	];

    function HasRankOrIsOwnerDirective(Logger, $rootScope, $parse) {

        return {
			restrict: 'A',
			scope: {
			   hasRankOrIsOwner: '='
			},

			link: function ($scope, elem, attrs) {

				if(typeof $scope.hasRankOrIsOwner !== 'object') {
					Logger.error('hasRankOrIsOwner directive requires object with rank and owner property');
				}

				$rootScope.$watch('currentUser', function() {
					check();
				});

				$scope.$watch(attrs.hasRankOrIsOwner, function() {
					check();
				}, true); // deep watcher

				var check = function() {
					Logger.info('Checking for rank: ' + $scope.hasRankOrIsOwner.rank +
							' or owner: ' + $scope.hasRankOrIsOwner.owner);
					if($rootScope.currentUser && $rootScope.currentUser.rank >= $scope.hasRankOrIsOwner.rank) {
						Logger.info('Rank sufficient: ' + $rootScope.currentUser.rank);
						elem.show();
					} else if($rootScope.currentUser &&
							$rootScope.currentUser.username === $scope.hasRankOrIsOwner.owner) {
						Logger.info('Owner found: ' + $rootScope.currentUser.username);
						elem.show();
					} else {
						elem.hide();
					}
				};

			}

        };

    }

})();