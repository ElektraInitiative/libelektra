'use strict';

module.exports = function (Logger, $http, $q, config) {

    var service = this;

    this.convert = function (parameters) {

        return $http.post(config.backend.root + 'conversion', parameters, {
            // custom options
        });

    };

    Logger.info('Conversion service ready!');

};
