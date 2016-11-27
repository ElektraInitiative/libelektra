'use strict';

module.exports = function($scope, Logger, $interval, news) {

	var vm = this;

	news = news.filter(function(elem) {
		return elem.type === 'file';
	});
	if(news.length > 5) {
		$scope.news = news.slice(0, 5);
	} else {
		$scope.news = news;
	}

	$scope.slogan = {
		adjectives: [
			'finally',
			'easily',
			'simply',
			'globally',
			'validated',
			'correctly',
			'fast',
			'specified',
			'sharable',
			'now'
		]
	};
	$scope.slogan.index = 0;

	function changeSlogan() {
		if ($scope.slogan.index >= $scope.slogan.adjectives.length - 1) {
			$scope.slogan.index = 0;
		} else {
			$scope.slogan.index++;
		}
	}

	$interval(function() {
		changeSlogan();
	}, 1500);

	Logger.info("Home controller ready");

};