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
            'shareable',
            'validated',
            'specified',
            'correctly',
            'simply',
            'easily',
            'now'
        ].sort(function (l, r) {
            return l.length > r.length;
        })
    };
    $scope.slogan.index = 0;
    $scope.slogan.directionUp = true;

    function changeSlogan() {
        if ($scope.slogan.directionUp === true) {
            $scope.slogan.index++;
        } else {
            $scope.slogan.index--;
        }

        // swap rotation direction
        if ($scope.slogan.index >= $scope.slogan.adjectives.length - 1 ||
                $scope.slogan.index <= 0) {
            $scope.slogan.directionUp = !$scope.slogan.directionUp;
        }
    }

    $interval(function () {
        changeSlogan();
    }, config.website.slogan.rotation_interval);

    Logger.info("Home controller ready");

};
