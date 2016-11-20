'use strict';

module.exports = function($translateProvider, config) {

	// configure translations
	$translateProvider
		.useSanitizeValueStrategy('sanitizeParameters')
		.useStaticFilesLoader({
			prefix: 'assets/translations/',
			suffix: '.json'
		})
		.registerAvailableLanguageKeys(config.translations.enabled, config.translations.mappings)
		.fallbackLanguage(config.translations.default)
		.preferredLanguage(config.translations.default);

};