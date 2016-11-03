'use strict';

module.exports = function($scope, Logger, news) {

	var vm = this;

	if(news.length > 5) {
		$scope.news = news.slice(0, 5);
	} else {
		$scope.news = news;
	}

	Logger.info("Home controller ready");

};