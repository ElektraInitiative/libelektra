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

			markedProvider.setRenderer({
				table: function(head, body) {
					return '<table class="table table-striped table-bordered">' +
								'<thead>' + head + '</thead>' +
								'<tbody>' + body + '</tbody>' +
							'</table>';
				}
			});

        }
    ]);

})();