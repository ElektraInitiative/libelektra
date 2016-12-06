'use strict';

module.exports = function ($scope, Logger, $state, $http, Notification, config) {

    var vm = this;

    $scope.user = {};

    this.doRegistration = function () {
        $http.post(config.backend.root + 'user', $scope.user, {
            // custom options
        }).then(function (response) {
            Logger.info('Successful registration!');
            Notification.success({
                title: 'APP.AUTH.REGISTRATION.NOTIFICATION.HEADER',
                message: 'APP.AUTH.REGISTRATION.NOTIFICATION.MESSAGE.' + response.data.i18n
            });
            $state.go('main.auth.login');
        }, function (response) {
            Logger.info('Failed registration!');
            Notification.error({
                title: 'APP.AUTH.REGISTRATION.NOTIFICATION.HEADER',
                message: 'APP.AUTH.REGISTRATION.NOTIFICATION.MESSAGE.' + response.data.i18n
            });
        });
    };

    Logger.info("Registration controller ready");

};
