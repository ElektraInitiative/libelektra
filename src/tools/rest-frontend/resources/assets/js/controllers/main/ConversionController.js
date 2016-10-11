(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .controller('ConversionController', ConversionController);

    ConversionController.$inject = [
		'$scope', 'Logger', 'formats', 'ConversionService', 'ReportService', 'Notification'
	];

    function ConversionController($scope, Logger, formats, ConversionService, ReportService, Notification) {

        var vm = this;

		$scope.parameters = {
			input: {
				format: {},
				snippet: ''
			},
			output: {
				format: {},
				snippet: '',
				validated: false
			}
		};
		$scope.formats = formats;
		$scope.lastError = '';

		this.doConversion = function() {
			// clear output field first
			$scope.parameters.output.snippet = '';

			var request = {};
			angular.copy($scope.parameters, request);
			request.input.format = $scope.parameters.input.format.plugin;
			request.output.format = $scope.parameters.output.format.plugin;
			delete request.output.snippet;
			delete request.output.validated;

			ConversionService.convert(request)
			.then(function(response) {
				$scope.parameters.output.snippet = response.data.output.snippet;
				$scope.parameters.output.validated = response.data.output.validated;
				Notification.success({
                    title: 'APP.CONVERSION.NOTIFICATION.HEADER',
                    message: 'APP.CONVERSION.NOTIFICATION.MESSAGE.' + response.data.i18n,
					delay: 10000
                });
			}, function(response) {
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

		this.clearFields = function() {
			$scope.parameters.input.format = $scope.formats[0];
			$scope.parameters.output.format = $scope.formats[0];
			$scope.parameters.input.snippet = '';
			$scope.parameters.output.snippet = '';
			$scope.parameters.output.validated = false;
		};

		this.createGithubIssue = function() {
			var title = 'Convert with plugin ' + $scope.parameters.input.format.plugin +
						' to ' + $scope.parameters.output.format.plugin;
			var message = 'I tried to convert a snippet on the website, but got the error: ' +
						$scope.lastError + '\n\n' +
						'## Used plugins\n' +
						'```\n' +
						'Input:\n' +
						'- Plugin: ' + $scope.parameters.input.format.plugin + '\n' +
						'- Format: ' + $scope.parameters.input.format.format + '\n' +
						'Output:\n' +
						'- Plugin: ' + $scope.parameters.output.format.plugin + '\n' +
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

    }

})();