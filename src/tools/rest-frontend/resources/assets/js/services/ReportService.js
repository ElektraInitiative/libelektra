(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .service('ReportService', ReportService);

    ReportService.$inject = [
        'Logger', '$window', 'config'
    ];

    function ReportService(Logger, $window, config) {

        var service = this;

		this.reportIssue = function(title, message, labels) {

			var url = config.github.website.root + config.github.website.paths.issues +
					'?title=' + encodeURIComponent(title) +
					'&body=' + encodeURIComponent(message);
			labels.forEach(function(elem) {
				url += '&labels[]=' + encodeURIComponent(elem);
			});

			$window.open(url , '_blank');

		};

		Logger.info('Report service ready!');

    }

})();