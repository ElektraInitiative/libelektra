(function() {

    'use strict';

    angular.module('elektra.rest.angular').config([
        '$stateProvider',
        '$urlRouterProvider',
        '$locationProvider',
        function($stateProvider, $urlRouterProvider, $locationProvider) {

            // configure html5mode for URLs (no #)
            $locationProvider.html5Mode({
                enabled: false
            });

            // configure default route
            $urlRouterProvider.otherwise("/home");

            // configure application states
            $stateProvider
                .state('main', {
                    abstract: true,
                    templateUrl: 'pages/main/template.html',
                    controller: 'MainController as ctrl'
                })
                .state('main.auth', {
                    abstract: true,
					url: '/auth',
                    templateUrl: "pages/main/auth/template.html",
                    ncyBreadcrumb: {
                        parent: 'main.home'
                    }
                })
                .state('main.auth.login', {
                    url: "/login",
                    templateUrl: "pages/main/auth/login.html",
                    controller: "AuthLoginController as ctrl",
                    ncyBreadcrumb: {
                        label: 'APP.BREADCRUMBS.MAIN.AUTH.LOGIN',
						parent: 'main.auth'
                    }
                })
				.state('main.auth.logout', {
					url: "/logout",
					controller: ['$rootScope', '$auth', '$state', 'Notification',
							function($rootScope, $auth, $state, Notification) {

						$auth.logout().then(function() {
							// Flip authenticated to false so that we no longer
							// show UI elements dependant on the user being logged in
							$rootScope.authenticated = false;
							$rootScope.currentUser = {};

							Notification.success({
								title: 'APP.AUTH.LOGOUT.NOTIFICATION.HEADER',
								message: 'APP.AUTH.LOGOUT.NOTIFICATION.MESSAGE.SUCCESS'
							});
							$state.go('main.home');
						});

					}]
				})
				.state('main.auth.register', {
					url: "/register",
					templateUrl: "pages/main/auth/registration.html",
					controller: "AuthRegistrationController as ctrl",
					ncyBreadcrumb: {
						label: 'APP.BREADCRUMBS.MAIN.AUTH.REGISTER',
						parent: 'main.auth'
					}
				})
                .state('main.home', {
                    url: '/home',
                    templateUrl: 'pages/main/home.html',
                    controller: 'HomeController as ctrl',
                    ncyBreadcrumb: {
                        label: 'APP.BREADCRUMBS.MAIN.HOME'
                    }
                })
				.state('main.account', {
					url: '/account',
					templateUrl: 'pages/main/users/details.html',
					controller: 'UserDetailsController as ctrl',
					params: {
						username: '',
						useAuthUser: true
					},
					resolve: {
						user: ['$stateParams', 'UserService', function($stateParams, UserService) {
							return UserService.get('', $stateParams.useAuthUser);
						}]
					},
					ncyBreadcrumb: {
						label: 'APP.BREADCRUMBS.MAIN.ACCOUNT',
						parent: 'main.home'
					}
				})
				.state('main.documentation', {
					url: '/documentation',
					templateUrl: 'pages/main/documentation/template.html',
					controller: 'DocumentationController as ctrl',
					resolve: {
						docs: ['DocumentationService', function(DocService) {
							return DocService.loadDocumentations();
						}],
						doc: ['DocumentationService', 'docs', function(DocService, docs) {
							return DocService.loadDocument(docs[0].url);
						}]
					},
					ncyBreadcrumb: {
						label: 'APP.BREADCRUMBS.MAIN.DOCUMENTATION.TEMPLATE',
						parent: 'main.home'
					}
				})
				.state('main.tutorials', {
					url: '/tutorials',
					templateUrl: 'pages/main/documentation/template.html',
					controller: 'DocumentationController as ctrl',
					resolve: {
						docs: ['DocumentationService', function(DocService) {
							return DocService.loadTutorials();
						}],
						doc: ['DocumentationService', 'docs', function(DocService, docs) {
							return DocService.loadDocument(docs[0].url);
						}]
					},
					ncyBreadcrumb: {
						label: 'APP.BREADCRUMBS.MAIN.TUTORIALS.TEMPLATE',
						parent: 'main.home'
					}
				})
				.state('main.conversion', {
					url: '/conversion',
					templateUrl: 'pages/main/conversion.html',
					controller: 'ConversionController as ctrl',
					resolve: {
						formats: ['EntryService', function(EntryService) {
							return EntryService.loadAvailableFormats();
						}]
					},
					ncyBreadcrumb: {
						label: 'APP.BREADCRUMBS.MAIN.CONVERSION',
						parent: 'main.home'
					}
				})
				.state('main.entries', {
					abstract: true,
					url: '/entries',
					templateUrl: 'pages/main/entries/template.html',
					ncyBreadcrumb: {
						parent: 'main.home'
					}
				})
                .state('main.entries.search', {
                    url: '/search',
                    templateUrl: 'pages/main/entries/search.html',
                    controller: 'EntrySearchController as ctrl',
                    ncyBreadcrumb: {
                        label: 'APP.BREADCRUMBS.MAIN.ENTRIES.SEARCH',
                        parent: 'main.entries'
                    }
                })
                .state('main.entries.details', {
                    url: '/details/:entry',
                    templateUrl: 'pages/main/entries/details.html',
                    controller: 'EntryDetailsController as ctrl',
					params: {
						entry: ''
					},
					resolve: {
						entry: ['$stateParams', 'EntryService', function($stateParams, EntryService) {
							return EntryService.get($stateParams.entry);
						}]
					},
                    ncyBreadcrumb: {
                        label: 'APP.BREADCRUMBS.MAIN.ENTRIES.DETAILS',
                        parent: 'main.entries.search'
                    }
                })
				.state('main.entries.edit', {
					url: '/edit/:entry',
					templateUrl: 'pages/main/entries/edit.html',
					controller: 'EntryEditController as ctrl',
					params: {
						entry: ''
					},
					resolve: {
						formats: ['EntryService', function(EntryService) {
							return EntryService.loadAvailableFormats();
						}],
						entry: ['$stateParams', 'EntryService', function($stateParams, EntryService) {
							return EntryService.get($stateParams.entry);
						}],
						hasPermission: ['$rootScope', 'entry', '$q', function($rootScope, entry, $q) {
							var deferred = $q.defer();

							if($rootScope.currentUser &&
									($rootScope.currentUser.rank >= 50 ||
									 $rootScope.currentUser.username === entry.meta.author))
							{
								deferred.resolve(true);
							} else {
								deferred.reject(false);
							}

							return deferred.promise;
						}]
					},
					data: {
						// rank: 50	// this check has to be done by the resolver as well
					},
					ncyBreadcrumb: {
						label: 'APP.BREADCRUMBS.MAIN.ENTRIES.EDIT',
						parent: 'main.entries.details'
					}
				})
                .state('main.entries.create', {
                    url: '/create',
                    templateUrl: 'pages/main/entries/create.html',
                    controller: 'EntryCreateController as ctrl',
					resolve: {
						formats: ['EntryService', function(EntryService) {
							return EntryService.loadAvailableFormats();
						}]
					},
                    data: {
                        rank: 10
                    },
                    ncyBreadcrumb: {
                        label: 'APP.BREADCRUMBS.MAIN.ENTRIES.CREATE',
                        parent: 'main.entries'
                    }
                })
				.state('main.users', {
					abstract: true,
					url: '/users',
					templateUrl: 'pages/main/users/template.html',
					ncyBreadcrumb: {
						parent: 'main.home'
					}
				})
                .state('main.users.search', {
                    url: '/search',
                    templateUrl: 'pages/main/users/search.html',
                    controller: 'UserSearchController as ctrl',
					data: {
						rank: 100
					},
                    ncyBreadcrumb: {
                        label: 'APP.BREADCRUMBS.MAIN.USERS.SEARCH',
                        parent: 'main.users'
                    }
                })
				.state('main.users.details', {
					url: '/details/:username',
					templateUrl: 'pages/main/users/details.html',
					controller: 'UserDetailsController as ctrl',
					params: {
						username: '',
						useAuthUser: false
					},
					data: {
						rank: 100
					},
					resolve: {
						user: ['$stateParams', 'UserService', function($stateParams, UserService) {
							return UserService.get($stateParams.username);
						}]
					},
					ncyBreadcrumb: {
						label: 'APP.BREADCRUMBS.MAIN.USERS.DETAILS',
						parent: 'main.users'
					}
				});

        }
    ]);

})();