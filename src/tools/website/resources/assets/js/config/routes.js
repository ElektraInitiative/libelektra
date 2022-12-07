"use strict";

module.exports = [
  "$stateProvider",
  "$urlRouterProvider",
  "$locationProvider",
  "webStructure",
  function (
    $stateProvider,
    $urlRouterProvider,
    $locationProvider,
    webStructure
  ) {
    var self = this;

    // configure html5mode for URLs (no #)
    $locationProvider.html5Mode({ enabled: true });

    // configure default route
    $urlRouterProvider.when("", "/home");
    $urlRouterProvider.when("/", "/home");
    $urlRouterProvider.otherwise("/error/404");

    // configure application states
    $stateProvider
      .state("main", {
        abstract: true,
        templateUrl: "pages/main/template.html",
        controller: "MainController as ctrl",
      })
      .state("main.error", {
        abstract: true,
        url: "/error",
        templateUrl: "pages/main/error/template.html",
        ncyBreadcrumb: {
          label: "APP.BREADCRUMBS.MAIN.ERROR",
          parent: "main.home",
        },
      })
      .state("main.error.404", {
        url: "/404",
        templateUrl: "pages/main/error/404.html",
        ncyBreadcrumb: {
          label: "APP.BREADCRUMBS.MAIN.ERROR.404",
          parent: "main.error",
        },
      })
      .state("main.home", {
        url: "/home",
        templateUrl: "pages/main/website/home.html",
        controller: "WebsiteHomeController as ctrl",
        ncyBreadcrumb: { label: "APP.BREADCRUMBS.MAIN.HOME" },
      })
      .state("main.news", {
        url: "/news/:file",
        templateUrl: "pages/main/website/news.html",
        controller: "WebsiteListfilesController as ctrl",
        params: { file: null },
        resolve: {
          files: [
            "news",
            function (news) {
              return news;
            },
          ],
          currentFile: [
            "$q",
            "$timeout",
            "$state",
            "$stateParams",
            "WebsiteService",
            "files",
            function (
              $q,
              $timeout,
              $state,
              $stateParams,
              WebsiteService,
              files
            ) {
              var deferred = $q.defer();

              $timeout(function () {
                if ($stateParams.file === null) {
                  $state.go(
                    "main.news",
                    {
                      file: files.filter(function (elem) {
                        return elem.type === "file";
                      })[0].slug,
                    },
                    { location: "replace" }
                  );
                  deferred.reject();
                } else {
                  var filtered = files.filter(function (elem) {
                    return (
                      elem.type === "file" && elem.slug === $stateParams.file
                    );
                  });
                  if (filtered.length === 0) {
                    $state.go(
                      "main.news",
                      { file: files[0].slug },
                      { location: "replace" }
                    );
                    deferred.reject();
                  } else {
                    WebsiteService.loadFile(filtered[0].file).then(function (
                      data
                    ) {
                      var file = filtered[0];
                      file.content = data;
                      deferred.resolve(file);
                    });
                  }
                }
              });

              return deferred.promise;
            },
          ],
        },
        ncyBreadcrumb: {
          label: "APP.BREADCRUMBS.MAIN.NEWS",
          parent: "main.home",
        },
      })
      .state("main.dyn", {
        abstract: true,
        template: "<div ui-view></div>",
        ncyBreadcrumb: { parent: "main.home" },
      });

    /* CONFIGURE DYNAMIC PAGES */

    // read the structure file and handle entries
    webStructure.forEach(function (entry) {
      consumeEntry(entry);
    });

    // main parse function
    function consumeEntry(entry) {
      switch (entry.type) {
        case "submenu":
          entry.children.forEach(function (child) {
            consumeEntry(child);
          });
          break;
        case "listfiles":
          $stateProvider.state("main.dyn." + entry.ref, {
            url: "/" + entry.ref + "/:file",
            templateUrl: "pages/main/website/listfiles.html",
            controller: "WebsiteListfilesController as ctrl",
            params: { file: null },
            data: { name: entry.name, ref: entry.ref },
            resolve: {
              files: [
                function () {
                  return entry.children;
                },
              ],
              currentFile: [
                "$q",
                "$timeout",
                "$state",
                "$stateParams",
                "WebsiteService",
                "files",
                function (
                  $q,
                  $timeout,
                  $state,
                  $stateParams,
                  WebsiteService,
                  files
                ) {
                  var deferred = $q.defer();

                  $timeout(function () {
                    if ($stateParams.file === null) {
                      $state.go(
                        "main.dyn." + entry.ref,
                        {
                          file: files.filter(function (elem) {
                            return elem.type === "file";
                          })[0].slug,
                        },
                        { location: "replace" }
                      );
                      deferred.reject();
                    } else {
                      var filtered = files.filter(function (elem) {
                        return (
                          elem.type === "file" &&
                          elem.slug === $stateParams.file
                        );
                      });
                      if (filtered.length === 0) {
                        $state.go(
                          "main.dyn." + entry.ref,
                          {
                            file: files.filter(function (elem) {
                              return elem.type === "file";
                            })[0].slug,
                          },
                          { location: "replace" }
                        );
                        deferred.reject();
                      } else {
                        WebsiteService.loadFile(filtered[0].options.path).then(
                          function (data) {
                            var file = filtered[0];
                            file.content = data;
                            deferred.resolve(file);
                          }
                        );
                      }
                    }
                  });

                  return deferred.promise;
                },
              ],
            },
            ncyBreadcrumb: {
              label: "APP.BREADCRUMBS.MAIN.DYN." + entry.ref.toUpperCase(),
              parent: "main.dyn",
            },
          });
          break;
        default:
          break;
      }
    }
  },
];
