# Elektra REST Frontend #

## Introduction ##

This document aims to provide information about Elektras `rest-frontend`, which is the frontend of the `rest-backend` allowing for search and sharing of configuration snippets. Besides that functionality, the frontend also contains the Elektra website.

## Design and Structure ##

The frontend is developed as single-page application (SPA) in [AngularJS (v1.5)](https://angularjs.org/). All dependencies are either already contained in the application project or (preferred) resolved through [npm](https://www.npmjs.com/) during installation (requires active internet connection). Compiling (browserification, concatenation & minification), as well as other tasks like running a lightweight webserver are handled by the task runner [grunt](http://gruntjs.com/).

### Directory Structure ###

The application project itself is mainly splitted into two directories: `resources` and `public`, whereas only the latter can directly been accessed by clients if the built-in `grunt server` is used to deploy the project.

The [resources](resources) directory contains the JavaScript source files, custom grunt tasks as well as the LESS files which are compiled into CSS files for the website.

The [public](public) directory contains HTML template files, assets like fonts, compiled JS and CSS files, as well as translation files and all dependencies resolved by `npm`, which are copied by `grunt`. It does also contain copied documentation files for the website.

### Part 1: Snippet Sharing ###

Sharing of snippets will require authentication, therefore registration and login were implemented. Snippets can be looked up by using the search function, which offers some convenience options like a filter and sorting. Snippets themselves can then be viewed in any supported format, downloaded, copied, etc. The snippets API is readable without authentication, but does require authentication for write operations (insert, update & delete).

Besides the snippet sharing functionality, the whole frontend implements a basic user system along with a permission system, allowing for higher roles with more privileges. This is necessary to be able to moderate the database, if necessary (spam protection).

### Part 2: Elektra Website ###

One part of the frontend is the new Elektra website containing documentation, tutorials and other important artifacts like news. Almost all necessary resources are generated and copied from a local repository clone to the website deployment during build (to refresh the website, a new build is necessary).

#### Important facts ####

- Links are internal on the website if the target is part of it too, otherwise they are external (i.e. linked to repo on external site).
- The frontend supports Markdown syntax via a plugin ([marked](https://github.com/chjj/marked)).
- Styling can then be done easily through LESS/CSS.
- The website structure is dynamically adjustable. There is a set of types which can be used to define links, menus, content of sites, etc. A detailed discussion for the website structure happened in #1015.

#### Limitations ####

**Global search:**
It is not planned to implement a global search for the website itself (documentation, tutorials, ...) as the resources are not easily searchable (static files, everything happens in the browser, i.e. pre-fetch of all files would be necessary for a search). The available search will only search the configuration snippet database (and users, for admins).

**AngularJS 2:**
At the time of development start, there was no stable AngularJS 2 release available yet, only early previews. Because of that, the frontend was developed using AngularJS 1.5. This shouldn't be seen as drawback, but rather as advantage as there are a lot more modules (plugins) available for Angular 1.5 right now than for version 2.

## Compiling and Installing ##

### Dependencies ###

The project has quite a few dependencies, of which most can be resolved automatically by the used package manager. The only dependency that has to be installed beforehand is the package manager [npm](https://www.npmjs.com/) itself, which comes bundled with [Node.js](https://nodejs.org/) (preferred installation).

### Compiling ###

The `rest-frontend` has full CMake integration, but does actually only two things:
- Install (copy) the project files to a target directory.
- Run `npm install` in this target directory, which does
 - resolve all `npm` dependencies (into the directory [node_modules](node_modules)).
 - run `grunt full` to compile all application sources ([resources](resources) dir) into working production files ([public](public) dir) and copy required `npm` dependencies in the `public` folder.

### Installing ###

It is not necessary to install anything by hand, CMake does this job already. If changes are made to the source files in [resources](resources), it is sufficient to run `grunt full` to build the application again. During development, it can be handy to use `grunt watch` to run a watcher daemon that re-compiles LESS or JS files whenever a change was made in the respective [resources](resources) directory.

## Run and Configure ##

The application allows for some basic configuration. Under normal circumstances it is sufficient to change the [application-config.json](application-config.json) in the root directory. It contains the URL to the backend, some URLs for GitHub resources and translation, as well as logger settings. Any change of this configuration does require to re-run `grunt full` in order to re-compile the project.

To run the application, basically two options are available:
- Use the built-in webserver of `grunt`, which can be configured in the [Gruntfile.js](Gruntfile.js) and run by `grunt server` (in the installation target directory).
- Use an own webserver to distribute the application. In order to do so, first `grunt full` should be run. After that, the content of the [public](public) directory can be copied to any location that suits the needs. `npm` dependencies in the [node_modules](node_modules) directory and the [resources](resources) directory are only necessary for development, but can be ignored for deployment.
  In order to not recieve any 404 errors by the webserver, it should redirect all requests that do not have a static file as target to the `index.html`.

### application-config.json ###

#### APIs (Backend & GitHub) ####

The configuration file allows to set the URL to the backend in `backend.root`. GitHub settings may be done in `github`.

#### Translations ####

The configuration file also allows to specify available translations in `translations.enabled`. To add a translation, copy an existing translation file in [public/assets/translations](public/assets/translations), translate it and add the name of the new language to the list in `translations.enabled`. After that run `grunt full` to re-compile the application.

If necessary, mappings for dialects as well as a default language can be specified as well.

#### Logger ####

It is possible to enable the frontend logger by changing `logger.enabled` in the configuration file.

## Development ##

When attempting to change the AngularJS application, it can be useful to first have a look at all used dependencies, which are listed in [resources/assets/js/application.js](resources/assets/js/application.js). After that, the configuration files in [resources/assets/js/config](resources/assets/js/config) should be checked. Probably the most important configuration is the router in [resources/assets/js/config/routes.config.js](resources/assets/js/config/routes.config.js).

### Life Cycle ###

An AngularJS application is bootstrapped by first instantiating constants (can be used for configuration). After that, service providers are run, which allows for further configuration of services. When the bootstrap process is finished and all services are instantiated based on the settings made within the service providers, the router will load the default route (main page) and bind the appropriate controller to it. Controllers are destroyed as soon as a page is changed, but services are not. So caching across pages can be done using services. AngularJS also allows for dependency injection in basically every part of the application (services, controllers, etc) by type-hinting the dependency name.

For detailed information, the website of [Angular](https://angularjs.org/) should be visited.

### Task configuration ###

All `grunt` tasks can be configured using the [Gruntfile.js](Gruntfile.js) in the application root directory.

### Code formatting ###

The task `grunt jshint` can be used to check the code formatting of JS source files.
