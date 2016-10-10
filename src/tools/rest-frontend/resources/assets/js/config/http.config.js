(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .constant('config.rest.basepath', 'http://localhost:8080/')
		.constant('config.github.apipath', 'https://api.github.com/')
		.constant('config.github.issuepath', 'https://github.com/ElektraInitiative/libelektra/issues/new')
		.constant('config.github.docpath', 'repos/ElektraInitiative/libelektra/contents/doc')
		.constant('config.github.tutpath', 'repos/ElektraInitiative/libelektra/contents/doc/tutorials')

		.config([
			'$httpProvider',
			'$provide',
			function($httpProvider, $provide) {

				$provide.factory('handleServerNotReachable', [
					'$q',
					'$injector',
					function($q, $injector) {

						return {

							responseError: function(rejection) {

								var Notification = $injector.get('Notification');

								if(rejection.status === -1) {
									Notification.error({
										title: 'APP.GLOBAL.NOTIFICATION.HEADER.CONNECTION_ISSUE',
										message: 'APP.GLOBAL.NOTIFICATION.MESSAGE.SERVER_NOT_REACHABLE',
										delay: 10000
									});
								}

								return $q.reject(rejection);

							}

						};

					}
				]);

				$httpProvider.interceptors.push('handleServerNotReachable');

			}
		]);

})();