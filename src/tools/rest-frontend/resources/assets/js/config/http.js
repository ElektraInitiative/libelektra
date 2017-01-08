'use strict';

module.exports = function ($httpProvider, $provide) {

    $provide.factory('handleServerNotReachable', [
        '$q',
        '$injector',
        function ($q, $injector) {

            return {
                responseError: function (rejection) {

                    var Notification = $injector.get('Notification');

                    if (rejection.status === -1) {
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

    // http interceptor that deletes expired auth tokens
    $provide.factory('handleAuthTokenExpired', [
        '$q',
        '$injector',
        function ($q, $injector) {
            return {
                responseError: function (rejection) {

                    var $auth = $injector.get('$auth');

                    if($auth.isAuthenticated() &&
                            (rejection.status === 401 && typeof rejection.data.i18n !== 'undefined' &&
                            ['NEED_AUTHENTICATION', 'USER_INSUFFICIENT_PERMISSIONS'].indexOf(rejection.data.i18n) > -1) ||
                            (rejection.status === 400 && typeof rejection.data.i18n !== 'undefined' &&
                            ['NO_CURRENT_USER'].indexOf(rejection.data.i18n) > -1)) {
                        // the token seems expired, let us delete it
                        $auth.logout();
                    }

                    return $q.reject(rejection);
                    
                }
            };
        }
    ]);

    $httpProvider.interceptors.push('handleServerNotReachable');
    $httpProvider.interceptors.push('handleAuthTokenExpired');

};
