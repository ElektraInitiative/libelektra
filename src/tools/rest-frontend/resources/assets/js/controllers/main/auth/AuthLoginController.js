(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .controller('AuthLoginController', AuthLoginController);

    AuthLoginController.$inject = [
        '$scope', 'Logger', '$state', '$auth', 'Notification'
    ];

    function AuthLoginController($scope, Logger, $state, $auth, Notification) {

        var vm = this;

        $scope.user = {};


        this.doLogin = function() {
            $auth.login($scope.user, {
				// custom options
			})
			.then(function(response) {
				Logger.info('Successful login!');
				Notification.success({
					title: 'APP.AUTH.LOGIN.NOTIFICATION.HEADER',
					message: 'APP.AUTH.LOGIN.NOTIFICATION.MESSAGE.SUCCESS'
				});
				$state.go('main.home');
			})
			.catch(function(response) {
				Logger.info('Failed login!');
				Notification.error({
					title: 'APP.AUTH.LOGIN.NOTIFICATION.HEADER',
					message: 'APP.AUTH.LOGIN.NOTIFICATION.MESSAGE.' + response.data.i18n
				});
			});
        };

        Logger.info("Login controller ready");

    }

})();