(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .service('ReportService', ReportService);

    ReportService.$inject = [
        'Logger', '$window', 'config.github.issuepath'
    ];

    function ReportService(Logger, $window, configGithubIssuepath) {

        var service = this;

		this.reportIssue = function(title, message, labels) {

			var url = configGithubIssuepath
					+ '?title=' + encodeURIComponent(title)
					+ '&body=' + encodeURIComponent(message);
			labels.forEach(function(elem) {
				url += '&labels[]=' + encodeURIComponent(elem);
			});

			$window.open(url , '_blank');

		};

    }

})();