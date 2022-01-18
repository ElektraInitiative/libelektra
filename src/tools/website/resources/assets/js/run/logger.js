"use strict";

module.exports = [
  "Logger",
  "config",
  function (Logger, config) {
    // configure logger
    Logger.debug = config.logger.enabled;
  },
];
