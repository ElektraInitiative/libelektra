'use strict';

module.exports = function($translateProvider, config) {

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

};