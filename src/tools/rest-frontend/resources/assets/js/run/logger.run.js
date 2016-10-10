(function() {

    'use strict';

    angular.module('elektra.rest.angular').run([
        'Logger',
        function(Logger) {

            // configure logger
            Logger.debug = true;

        }
    ]);

})();