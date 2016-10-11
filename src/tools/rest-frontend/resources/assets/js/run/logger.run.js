(function() {

    'use strict';

    angular.module('elektra.rest.angular').run([
        'Logger',
		'config',
        function(Logger, config) {

            // configure logger
            Logger.debug = config.logger.enabled;

        }
    ]);

})();