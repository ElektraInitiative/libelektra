'use strict';

module.exports = function ($rootScope, $scope, Logger, $state, EntryService) {

    var vm = this;

    $scope.options = {
        is_advanced: false,
        filter: $rootScope.entriesSearchString,
        filterby: {
            options: [
                {id: 'all', name: 'APP.ENTRIES.SEARCH.ADVANCED.FILTERBY.ALL'},
                {id: 'key', name: 'APP.ENTRIES.SEARCH.ADVANCED.FILTERBY.KEY'},
                {id: 'title', name: 'APP.ENTRIES.SEARCH.ADVANCED.FILTERBY.TITLE'},
                {id: 'description', name: 'APP.ENTRIES.SEARCH.ADVANCED.FILTERBY.DESCRIPTION'},
                {id: 'author', name: 'APP.ENTRIES.SEARCH.ADVANCED.FILTERBY.AUTHOR'},
                {id: 'tags', name: 'APP.ENTRIES.SEARCH.ADVANCED.FILTERBY.TAGS'}
            ]
        },
        sort: {
            options: [
                {id: 'asc', name: 'APP.ENTRIES.SEARCH.ADVANCED.SORT.ASC'},
                {id: 'desc', name: 'APP.ENTRIES.SEARCH.ADVANCED.SORT.DESC'}
            ]
        },
        sortby: {
            options: [
                {id: 'key', name: 'APP.ENTRIES.SEARCH.ADVANCED.SORTBY.KEY'},
                {id: 'title', name: 'APP.ENTRIES.SEARCH.ADVANCED.SORTBY.TITLE'},
                {id: 'created_at', name: 'APP.ENTRIES.SEARCH.ADVANCED.SORTBY.CREATED_AT'},
                {id: 'author', name: 'APP.ENTRIES.SEARCH.ADVANCED.SORTBY.AUTHOR'},
                {id: 'organization', name: 'APP.ENTRIES.SEARCH.ADVANCED.SORTBY.ORGANIZATION'},
                {id: 'application', name: 'APP.ENTRIES.SEARCH.ADVANCED.SORTBY.APPLICATION'},
                {id: 'scope', name: 'APP.ENTRIES.SEARCH.ADVANCED.SORTBY.SCOPE'},
                {id: 'slug', name: 'APP.ENTRIES.SEARCH.ADVANCED.SORTBY.SLUG'}
            ]
        },
        rows: {
            options: [10, 20, 50, 100, 200],
            value: 10
        },
        offset: 0
    };

    $scope.options.filterby.value = $scope.options.filterby.options[0];
    $scope.options.sort.value = $scope.options.sort.options[0];
    $scope.options.sortby.value = $scope.options.sortby.options[0];

    $scope.pagination = {
        currentPage: null,
        pageCount: null,
        pages: []
    };

    $scope.searchResult = {};

    $scope.$watch('options', function () {
        vm.loadEntries();
    }, true); // true for object deep-watching


    this.loadEntries = function () {
		$scope.options.filter = $rootScope.entriesSearchString;
        var params = {
            filter: $scope.options.filter,
            filterby: $scope.options.filterby.value.id,
            sort: $scope.options.sort.value.id,
            sortby: $scope.options.sortby.value.id,
            offset: $scope.options.offset,
            rows: $scope.options.rows.value
        };
        EntryService.search(params).then(function (data)
        {
            $scope.searchResult = data;
            vm.calculatePagination();
        });
    };

    this.calculatePagination = function () {
        Logger.info('Current offset: ' + $scope.searchResult.offset);
        var entries = $scope.searchResult.offset;
        entries += $scope.searchResult.elements;
        entries += $scope.searchResult.remaining;
        Logger.info('Current entries: ' + entries);
        var numPages = Math.floor(entries / $scope.options.rows.value);
        $scope.pagination.pageCount = numPages;
        Logger.info('Current page count: ' + numPages);

        var curPage = ($scope.searchResult.offset / $scope.options.rows.value) + 1;
        $scope.pagination.currentPage = curPage;
        Logger.info('Current page: ' + curPage);

        var pages = [];
        for (var i = 2; i >= 1; i--) {
            if (curPage - i > 0)
                pages.push(curPage - i);
        }
        pages.push(curPage);
        for (var j = 1; j <= 2; j++) {
            if (curPage + j <= numPages)
                pages.push(curPage + j);
        }
        $scope.pagination.pages = pages;
        Logger.info('Pages: ' + pages);
    };

    this.goToPage = function (index) {
        Logger.info('Go to page: ' + index);
        if (index === $scope.pagination.currentPage)
            return;
        if (index <= 0)
            return;
        var entries = $scope.searchResult.offset;
        entries += $scope.searchResult.elements;
        entries += $scope.searchResult.remaining;
        if (index > entries / $scope.options.rows.value)
            return;

        $scope.options.offset = (index - 1) * $scope.options.rows.value;
    };

    this.toggleAdvancedOptions = function () {
        $scope.options.is_advanced = !$scope.options.is_advanced;
    };

    this.toggleSorting = function (sortby) {
        var currentSortby = $scope.options.sortby.value;
        var currentSort = $scope.options.sort.value;
        var newSortby = $scope.options.sortby.options.filter(function (elem) {
            return elem.id === sortby;
        })[0];

        if (currentSortby.id === newSortby.id) {
            // only toggle sort direction
            $scope.options.sort.value = $scope.options.sort.options.filter(function (elem) {
                return elem.id !== currentSort.id;	// filter the same one and take the next
            })[0];
        } else {
            $scope.options.sortby.value = newSortby;
            $scope.options.sort.value = $scope.options.sort.options[0];
        }
    };

    this.goToEntry = function (entry) {
        Logger.info('Clicked entry: ' + JSON.stringify(entry));
        $state.go('main.entries.details', {'entry': entry});
    };

    Logger.info("Search ready");

};
