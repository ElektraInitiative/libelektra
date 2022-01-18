"use strict";

module.exports = [
  "Logger",
  "$http",
  "$q",
  "config",
  function (Logger, $http, $q, config) {
    var service = this;

    this.loadFile = function (url) {
      var deferred = $q.defer();

      $http
        .get(config.website.content_root + url, { skipAuthorization: true })
        .then(
          function (response) {
            deferred.resolve(response.data);
          },
          function (response) {
            deferred.reject(response.data);
          }
        );

      return deferred.promise;
    };

    Logger.info("Website service ready!");
  },
];
