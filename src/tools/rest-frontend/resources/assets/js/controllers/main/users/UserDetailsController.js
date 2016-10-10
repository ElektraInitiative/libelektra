(function() {

    'use strict';

    angular.module('elektra.rest.angular')
        .controller('UserDetailsController', UserDetailsController);

    UserDetailsController.$inject = [
        '$scope', 'Logger', 'Notification', 'UserService', '$stateParams', 'user'
    ];

    function UserDetailsController($scope, Logger, Notification, UserService, $stateParams, user) {

        var vm = this;

        $scope.user = user;

		$scope.ranks = [
			{id: 10, name: 'APP.USERS.DETAILS.LABEL.RANK.10'},
			{id: 50, name: 'APP.USERS.DETAILS.LABEL.RANK.50'},
			{id: 100, name: 'APP.USERS.DETAILS.LABEL.RANK.100'}
		];

		$scope.changes = {
			password: {
				input1: '',
				input2: ''
			},
			email: '',
			rank: $scope.ranks.filter(function(elem) {
				return elem.id === $scope.user.rank;
			})[0]
		};

		this.changePassword = function() {
			Logger.info('Attempting to change password');
			vm.executeChange({
				password: $scope.changes.password.input1
			}, $scope.passwordForm, function() {
				$scope.changes.password.input1 = '';
				$scope.changes.password.input2 = '';
			});
		};

		this.changeEmail = function() {
			Logger.info('Attempting to change email');
			vm.executeChange({
				email: $scope.changes.email
			}, $scope.emailForm, function() {
				$scope.user.email = $scope.changes.email;
				$scope.changes.email = '';
			});
		};

		this.changeRank = function() {
			Logger.info('Attempting to change rank');
			vm.executeChange({
				rank: $scope.changes.rank.id
			}, $scope.rankForm, function() {
				$scope.user.rank = $scope.changes.rank.id;
			});
		};

		this.executeChange = function(data, form, callback) {
			UserService.update($scope.user.username, data, $stateParams.useAuthUser).then(function(response) {
				callback();
				Notification.success({
					title: 'APP.USERS.DETAILS.NOTIFICATION.HEADER',
					message: 'APP.USERS.DETAILS.NOTIFICATION.MESSAGE.' + response.data.i18n
				});
			}, function(response) {
				Notification.error({
					title: 'APP.USERS.DETAILS.NOTIFICATION.HEADER',
					message: 'APP.USERS.DETAILS.NOTIFICATION.MESSAGE.' + response.data.i18n
				});
			});

			// set form as pristine so a change has to be made for further requests
			form.$setPristine();
		};

		Logger.info('Showing user infos for: ' + user);
        Logger.info("User details controller ready");

    }

})();