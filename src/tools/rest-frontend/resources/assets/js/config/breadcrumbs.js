'use strict';

module.exports = function ($breadcrumbProvider) {

    // configure breadcrumbs
    $breadcrumbProvider.setOptions({
        templateUrl: 'templates/breadcrumbs.html',
        prefixStateName: 'main'
    });

};
