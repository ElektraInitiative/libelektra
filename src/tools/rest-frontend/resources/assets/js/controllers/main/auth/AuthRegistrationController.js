(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .controller('AuthRegistrationController', AuthRegistrationController);

    AuthRegistrationController.$inject = [
        '$rootScope', '$scope', 'Logger', '$state', '$http', 'Notification', 'config.rest.basepath'
    ];

    function AuthRegistrationController($rootScope, $scope, Logger, $state, $http, Notification,
			configRestBasepath) {

        var vm = this;

        $scope.user = {};


        this.doRegistration = function() {
            $http.post(configRestBasepath + 'user', $scope.user, {
				// custom options
			})
			.then(function(response) {
				Logger.info('Successful registration!');
				Notification.success({
					title: 'APP.AUTH.REGISTRATION.NOTIFICATION.HEADER',
					message: 'APP.AUTH.REGISTRATION.NOTIFICATION.MESSAGE.' + response.data.i18n
				});
				$state.go('main.auth.login');
			}, function(response) {
				Logger.info('Failed registration!');
				Notification.error({
					title: 'APP.AUTH.REGISTRATION.NOTIFICATION.HEADER',
					message: 'APP.AUTH.REGISTRATION.NOTIFICATION.MESSAGE.' + response.data.i18n
				});
			});
        }

        Logger.info("Registration controller ready");

    }

})();