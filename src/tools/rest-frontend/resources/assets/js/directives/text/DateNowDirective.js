'use strict';

module.exports = function ($filter) {

    return function ($scope, element, attrs) {
        element.text($filter('date')(new Date(), attrs.dateNow));
    };

};
