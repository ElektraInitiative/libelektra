'use strict';

var angular = require('angular');

angular.module('elektra.rest.angular')
        .controller('EntryCreateController', require('./EntryCreateController'))
        .controller('EntryDeleteConfirmationController', require('./EntryDeleteConfirmationController'))
        .controller('EntryDetailsController', require('./EntryDetailsController'))
        .controller('EntryEditController', require('./EntryEditController'))
        .controller('EntrySearchController', require('./EntrySearchController'));
