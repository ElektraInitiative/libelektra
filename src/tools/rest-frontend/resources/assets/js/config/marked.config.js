(function() {

    'use strict';

    angular.module('elektra.rest.angular').config([
        'markedProvider',
        function(markedProvider) {

            markedProvider.setOptions({
				gfm: true,
				tables: true,
				highlight: function(code, lang) {
					if(lang) {
						return hljs.highlight(lang, code, true).value;
					} else {
						return hljs.highlightAuto(code).value;
					}
				}
			});

        }
    ]);

})();