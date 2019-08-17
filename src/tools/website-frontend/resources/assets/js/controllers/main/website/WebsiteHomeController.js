'use strict';

module.exports = function ($scope, Logger, $interval, config, news) {

    var vm = this;

    news = news.filter(function (elem) {
        return elem.type === 'file';
    });
    if (news.length > 5) {
        $scope.news = news.slice(0, 5);
    } else {
        $scope.news = news;
    }

    $scope.slogan = {
        adjectives: [
            'fast',
            'specified',
            'globally',
            'shareable',
            'now',
            'validated',
            'simply',
            'correctly',
            'easily',
            'finally'
        ]
    };

    Logger.info("Home controller ready");

};
