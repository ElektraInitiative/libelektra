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
            'finally',
            'globally',
            'sharable',
            'validated',
            'specified',
            'correctly',
            'simply',
            'easily',
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

    $interval(function () {
        changeSlogan();
    }, config.website.slogan.rotation_interval);

    Logger.info("Home controller ready");

};
