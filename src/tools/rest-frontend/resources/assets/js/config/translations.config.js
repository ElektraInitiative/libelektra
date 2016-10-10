(function() {

    'use strict';

    angular.module('elektra.rest.angular').config([
        '$translateProvider',
        function($translateProvider) {

            // configure translations
            $translateProvider
                .useSanitizeValueStrategy('sanitize')
                .useStaticFilesLoader({
                    prefix: 'assets/translations/',
                    suffix: '.json'
                })
                .registerAvailableLanguageKeys(['en'], {
                    'en_US': 'en',
                    'en_UK': 'en'
                })
                .fallbackLanguage('en')
                .preferredLanguage('en');

        }
    ]);

})();