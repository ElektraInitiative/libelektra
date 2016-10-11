(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .controller('DocumentationController', DocumentationController);

    DocumentationController.$inject = [
		'$scope', 'Logger', 'DocumentationService', 'docs', 'doc'
	];

    function DocumentationController($scope, Logger, DocumentationService, docs, doc) {

        var vm = this;

		$scope.docs = docs;
		$scope.currentDoc = doc;

		this.loadDocument = function(url) {
			DocumentationService.loadDocument(url).then(function(data) {
				$scope.currentDoc = data;
			}, function(error) {
				Logger.error('Could not load specific document');
			});
		};

		Logger.info("Documentation controller ready");

    }

})();