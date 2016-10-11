(function() {

    'use strict';

    angular.module('elektra.rest.angular').config([
        '$translateProvider',
		'config',
        function($translateProvider, config) {

            // configure translations
            $translateProvider
                .useSanitizeValueStrategy('sanitize')
                .useStaticFilesLoader({
                    prefix: 'assets/translations/',
                    suffix: '.json'
                })
                .registerAvailableLanguageKeys(config.translations.enabled, config.translations.mappings)
                .fallbackLanguage(config.translations.default)
                .preferredLanguage(config.translations.default);

        }
    ]);

})();