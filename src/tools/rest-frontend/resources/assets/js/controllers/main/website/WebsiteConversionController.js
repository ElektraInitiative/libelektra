'use strict';

var angular = require('angular');

module.exports = function ($scope, Logger, formats, ConversionService, ReportService, Notification) {

    var vm = this;

    $scope.parameters = {
        input: {
            format: {},
            formatconf: '',
            snippet: ''
        },
        output: {
            format: {},
            formatconf: '',
            snippet: '',
            validated: false
        }
    };
    $scope.formats = formats;
    $scope.formatsInput = formats.filter(function (elem) {
        return (elem.plugin.statuses.indexOf("writeonly") !== -1) ? false : true;
    });
    $scope.formatsOutput = formats.filter(function (elem) {
        return (elem.plugin.statuses.indexOf("readonly") !== -1) ? false : true;
    });
    $scope.lastError = '';

    this.doConversion = function () {
        // clear output field first
        $scope.parameters.output.snippet = '';

        var request = {};
        angular.copy($scope.parameters, request);

        request.input.format = $scope.parameters.input.format.plugin.name;
        if ($scope.parameters.input.formatconf !== '') {
            request.input.format += ' ' + $scope.parameters.input.formatconf;
        }
        request.output.format = $scope.parameters.output.format.plugin.name;
        if ($scope.parameters.output.formatconf !== '') {
            request.output.format += ' ' + $scope.parameters.output.formatconf;
        }

        delete request.input.formatconf;
        delete request.output.formatconf;
        delete request.output.snippet;
        delete request.output.validated;

        ConversionService.convert(request)
                .then(function (response) {
                    $scope.parameters.output.snippet = response.data.output.snippet;
                    $scope.parameters.output.validated = response.data.output.validated;
                    Notification.success({
                        title: 'APP.CONVERSION.NOTIFICATION.HEADER',
                        message: 'APP.CONVERSION.NOTIFICATION.MESSAGE.' + response.data.i18n,
                        delay: 10000
                    });
                }, function (response) {
                    Notification.error({
                        title: 'APP.CONVERSION.NOTIFICATION.HEADER',
                        message: 'APP.CONVERSION.NOTIFICATION.MESSAGE.' + response.data.i18n,
                        delay: 10000
                    });
                    $scope.lastError = response.data.i18n;
                });

        // set form as pristine so a change has to be made for further requests
        $scope.convertForm.$setPristine();
    };

    this.clearFields = function (pristine) {
        pristine = (typeof pristine === 'undefined') ? false : pristine;

        $scope.parameters.input.format = $scope.formatsInput[0];
        $scope.parameters.output.format = $scope.formatsOutput[0];
        $scope.parameters.input.snippet = '';
        $scope.parameters.output.snippet = '';
        $scope.parameters.output.validated = false;

        if (pristine === true) {
            $scope.convertForm.$setPristine();
        }
    };

    this.createGithubIssue = function () {
        var title = 'Convert with plugin ' + $scope.parameters.input.format.plugin.name +
                ' to ' + $scope.parameters.output.format.plugin.name;
        var message = 'I tried to convert a snippet on the website, but got the error: ' +
                $scope.lastError + '\n\n' +
                '## Used plugins\n' +
                '```\n' +
                'Input:\n' +
                '- Plugin: ' + $scope.parameters.input.format.plugin.name + '\n' +
                '- Format: ' + $scope.parameters.input.format.format + '\n' +
                'Output:\n' +
                '- Plugin: ' + $scope.parameters.output.format.plugin.name + '\n' +
                '- Format: ' + $scope.parameters.output.format.format + '\n' +
                '```\n\n' +
                '## Input configuration\n' +
                '```\n' +
                $scope.parameters.input.snippet + '\n' +
                '```\n\n' +
                '## Last output configuration\n' +
                '```\n' +
                $scope.parameters.output.snippet + '\n' +
                '```\n\n' +
                '## Additional information\n';
        var labels = ['website'];

        ReportService.reportIssue(title, message, labels);
    };

    this.clearFields();
    Logger.info("Conversion controller ready");

};
