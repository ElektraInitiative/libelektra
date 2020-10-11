"use strict";

module.exports = [
  "$uibModalInstance",
  "$scope",
  "Logger",
  "entry",
  function($uibModalInstance, $scope, Logger, entry) {
    var vm = this;

    $scope.entry = entry;

    this.ok = function() {
      $uibModalInstance.close(true);
    };

    this.abort = function() {
      $uibModalInstance.dismiss(false);
    };

    Logger.info("Entry delete confirmation controller ready");
  }
];
