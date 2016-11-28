'use strict';

var angular = require('angular');

angular.module('elektra.rest.angular')
        .controller('AuthLoginController', require('./AuthLoginController'))
        .controller('AuthRegistrationController', require('./AuthRegistrationController'));
