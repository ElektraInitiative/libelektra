'use strict';

module.exports = function (Logger, config) {

    // configure logger
    Logger.debug = config.logger.enabled;

};
