"use strict";

module.exports = [
  "$httpProvider",
  "$provide",
  function ($httpProvider, $provide) {
    $provide.factory("handleServerNotReachable", [
      "$q",
      "$injector",
      function ($q, $injector) {
        return {
          responseError: function (rejection) {
            var Notification = $injector.get("Notification");

            if (rejection.status === -1) {
              Notification.error({
                title: "APP.GLOBAL.NOTIFICATION.HEADER.CONNECTION_ISSUE",
                message: "APP.GLOBAL.NOTIFICATION.MESSAGE.SERVER_NOT_REACHABLE",
                delay: 10000,
              });
            }

            return $q.reject(rejection);
          },
        };
      },
    ]);

    $httpProvider.interceptors.push("handleServerNotReachable");
  },
];
