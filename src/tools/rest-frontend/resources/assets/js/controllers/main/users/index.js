'use strict';

var angular = require('angular');

angular.module('elektra.rest.angular')
        .controller('UserDetailsController', require('./UserDetailsController'))
        .controller('UserSearchController', require('./UserSearchController'));
