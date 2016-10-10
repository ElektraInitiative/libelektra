(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .service('DocumentationService', DocumentationService);

    DocumentationService.$inject = [
        'Logger', '$http', '$q', 'base64', '$filter', 'config.github.apipath',
		'config.github.docpath', 'config.github.tutpath'
    ];

    function DocumentationService(Logger, $http, $q, base64, $filter, configGithubApipath,
			configGithubDocpath, configGithubTutpath) {

        var service = this;

		this.cache = {
			documentation: {
				files: []
			},
			tutorials: {
				files: []
			}
		};


		this.loadFiles = function(path, cache) {

			var deferred = $q.defer();

			if(cache.files.length > 0) {
				deferred.resolve(cache.files);
			} else {
				$http.get(configGithubApipath + path, {
					skipAuthorization: true
				}).success(function(data) {
					data.forEach(function(elem) {
						if(elem.type === "file") {
							// skip no-doc files
							var no_doc_files = ['CMakeLists.txt', 'Doxyfile'];
							if(no_doc_files.indexOf(elem.name) === -1) {
								// change elem name to something more readable
								if(elem.name.indexOf('.') > -1) {
									elem.name_pretty = elem.name.substr(0, elem.name.indexOf('.'));
								} else {
									elem.name_pretty = elem.name;
								}
								// first capitalize: API -> Api, INSTALL -> Install
								elem.name_pretty = elem.name_pretty.replace('-', ' ');
								elem.name_pretty = $filter('firstCapitalize')(elem.name_pretty, true);
								cache.files.push(elem);
							}
						}
					});
					deferred.resolve(cache.files);
				}).error(function(data) {
					deferred.reject(data);
				});
			}

			return deferred.promise;

		};

		this.loadDocumentations = function() {
			return service.loadFiles(configGithubDocpath, service.cache.documentation);
		};

		this.loadTutorials = function() {
			return service.loadFiles(configGithubTutpath, service.cache.tutorials);
		};

		this.loadDocument = function(url) {

			var deferred = $q.defer();

			$http.get(url, {
				skipAuthorization: true
			}).success(function(data) {
				if(data.encoding === 'base64') {
					data.content_decoded = base64.decode(data.content);
				} else {
					data.content_decoded = data.content;
				}
				deferred.resolve(data);
			}).error(function(data) {
				deferred.reject(data);
			});

			return deferred.promise;

		};

    }

})();