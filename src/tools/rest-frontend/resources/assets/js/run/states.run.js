(function() {

    'use strict';

    angular.module('elektra.rest.angular').run([
        'Logger',
        '$rootScope',
        '$state',
        '$auth',
        'UserService',
        function(Logger, $rootScope, $state, $auth, UserService) {

            $rootScope.$on("$stateChangeError", console.log.bind(console));

            $rootScope.$on('$stateChangeStart', function(event, toState, toParams, fromState, fromParams) {

                Logger.info("Change state from '" + fromState.name + "' (" + fromParams + ") to '" + toState.name + "' (" + toParams + ")");
                Logger.info('From state: ' + JSON.stringify(fromState));
                Logger.info('To state: ' + JSON.stringify(toState));

                if($auth.isAuthenticated()) {

                    Logger.log("User is authenticated");

                    // if user is already logged in, go to dashboard
                    if (toState.name === "main.auth.login") {

                        // Preventing the default behavior allows us to use $state.go
                        // to change states
                        event.preventDefault();

                        Logger.info("Redirect to home");
                        $state.go('main.home');

                    }

                    // get user data of currently authenticated user
                    UserService.get('', true).then(function(data) {

						$rootScope.authenticated = true;
                        $rootScope.currentUser = data;
                        Logger.info('Current user: ' + JSON.stringify($rootScope.currentUser));

						// check if user has permissions for required route
						if (toState.data && toState.data.rank) {

							Logger.log("Check route permissions");

							if (!$auth.isAuthenticated() ||
									!$rootScope.currentUser ||
									$rootScope.currentUser.rank < toState.data.rank) {

								Logger.log("Insufficient permissions");

								event.preventDefault();
								if(fromState.abstract === true) {
									$state.go('main.home');
								}

							}

						}

                    }, function(data) {

						Logger.info('Could not load details for current user, go home');
						event.preventDefault();
						$state.go('main.home');

					});

                } else {

                    Logger.log("Not authenticated");

                    $rootScope.authenticated = false;
                    $rootScope.currentUser = null;

                }

            });

            // (re-)load user data on state changes
            $rootScope.$on('$stateChangeSuccess', function(event, toState, toParams, fromState, fromParams) {

                Logger.info("State change success!");

                $rootScope.currentState = toState.name;

            });

        }
    ]);

})();