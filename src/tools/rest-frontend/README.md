# Elektra REST Frontend #

## Introduction ##

This document aims to provide information about Elektras `@tool@`,
which is the frontend of the `rest-backend` allowing for search and sharing of configuration snippets.
Besides that functionality, the frontend also contains the Elektra website.

## Design and Structure ##

The frontend is developed as single-page application (SPA) in [AngularJS (v1.5)](https://angularjs.org/).
All dependencies are either already contained in the application project or (preferred) resolved through
the dependency manager [npm](https://www.npmjs.com/) during installation (requires active internet connection).
Compiling (browserification, concatenation & minification), as well as other tasks like running a lightweight
webserver are handled by the nodeJS based task runner [grunt](http://gruntjs.com/) (installed by npm).

## Run and Configure ##

The application allows for some basic configuration.
Under normal circumstances it is sufficient to change the
[application-config.json](application-config.json) in the root directory.
It contains the URL to the backend, some URLs for GitHub resources and translation,
as well as logger settings.
Any change of this configuration does require to re-run `grunt full` (or `kdb build-@tool@`)
in order to re-compile the project.

To run the application, basically two options are available:
- Use the built-in webserver of `grunt`, which can be configured in the
  [Gruntfile.js](Gruntfile.js) and run by `grunt server` (in the installation target directory)
  or `kdb run-@tool@` from anywhere.
- Use an own webserver to distribute the application.
  In order to do so, first `grunt full` (or `kdb build-@tool@`) should be run.
  After that, the content of the [public](public/) directory can be copied to any location
  that suits the needs. `npm` dependencies in the [node_modules](node_modules/) directory
  and the [resources](resources/) directory are only necessary for development,
  but can be ignored for deployment. The required dependencies were copied to the [public](public/)
  directory already.

In order to not receive any 404 errors by the webserver, it should serve the `index.html`
for all requests that do not have a static file as target.
The `index.html` will then try to serve the (dynamic) URL itself.

To stop the `@tool@`, simply run `kdb stop-@tool@` from anywhere.

### application-config.json ###

This file does not exist by default. It needs to be copied from the template `application-config.json.tpl`.
To copy the configuration template and mount it into the key database, execute
the command `kdb mount-rest-frontend-config`. The configuration will then be available below
`@config_root@/@config_default_profile@`.

#### APIs (Backend & GitHub) ####

The configuration file allows to set the URL to the backend in `backend.root`.
GitHub settings may be done in `github`.

#### Translations ####

The configuration file also allows to specify available translations in `translations.enabled`.
To add a translation, copy an existing translation file in
[public/assets/translations](public/assets/translations), translate it and add the name of
the new language to the list in `translations.enabled`.
After that run `grunt full` (or `kdb build-@tool@`) to re-compile the application.

If necessary, mappings for dialects as well as a default language can be specified as well.

#### Logger ####

It is possible to enable the frontend logger by changing `logger.enabled` in the configuration file.

### Directory Structure ###

The application project itself is mainly splitted into two directories: `resources` and `public`,
whereas only the latter can directly been accessed by clients if the built-in
`grunt server` is used to deploy the project.

The [resources](resources/) directory contains the JavaScript source files,
custom grunt tasks as well as the LESS files which are compiled into CSS files for the website.

The [public](public/) directory contains HTML template files, assets like fonts,
compiled JS and CSS files, as well as translation files and all dependencies resolved by `npm`,
which are copied by `grunt`. It does also contain copied documentation files for the website.

### Part 1: Snippet Sharing ###

Sharing of snippets will require authentication, therefore registration and login were implemented.
Snippets can be looked up by using the search function, which offers some convenience options
like a filter and sorting.
Snippets themselves can then be viewed in any supported format, downloaded, copied, etc.
The snippets API is readable without authentication, but does require authentication for
write operations (insert, update & delete).

Besides the snippet sharing functionality, the whole frontend implements a basic user system
along with a permission system, allowing for higher roles with more privileges.
This is necessary to be able to moderate the database, if necessary (spam protection).

### Part 2: Elektra Website ###

The second part of the frontend is the new Elektra website containing documentation,
tutorials and other important artifacts like news.
Almost all necessary resources are generated and copied from a local repository clone
to the website deployment during build (to refresh the website, a new build is necessary).

#### Important facts ####

- Links are internal on the website if the target is part of it too,
  otherwise they are external (i.e. linked to repo on external site).
- The frontend supports Markdown syntax via a plugin ([marked](https://github.com/chjj/marked)).
- Styling can then be done easily through LESS/CSS.
- The website structure is dynamically adjustable.
  There is a set of types which can be used to define links, menus, content of sites, etc.
  A detailed discussion for the website structure happened in #1015.

#### Limitations ####

**Global search:**
It is not planned to implement a global search for the website itself (documentation, tutorials, ...)
as the resources are not easily searchable (static files, everything happens in the browser,
i.e. pre-fetch of all files would be necessary for a search).
The available search will only search the configuration snippet database (and users, for admins).

**AngularJS 2:**
At the time of development start, there was no stable AngularJS 2 release available yet,
only early previews. Because of that, the frontend was developed using AngularJS 1.5.
This shouldn't be seen as drawback, but rather as advantage as there are a lot more
modules (plugins) available for Angular 1.5 right now than for version 2.

## Compiling and Installing ##

### Dependencies ###

The project has quite a few dependencies, of which most can be resolved automatically
by the used package manager.
The only dependency that has to be installed beforehand is the package manager
[npm](https://www.npmjs.com/) itself,
which comes bundled with [Node.js](https://nodejs.org/) (preferred installation).

### Compiling ###

The `@tool@` has full CMake integration, which does actually only two things:
- Install (copy) the project files to a target directory.
- Run `npm install` in this target directory, which does
 - resolve all `npm` dependencies (into the directory [node_modules](node_modules/)).
 - run `grunt full` to compile all application sources ([resources](resources/) dir)
   into working production files ([public](public/) dir) and
   copy required `npm` dependencies in the `public` folder.

### Installing ###

It is not necessary to install anything by hand, CMake does this job already.
If changes are made to the source files in [resources](resources/),
it is sufficient to run `grunt full` (or `kdb build-@tool@`) to build the application again.
During development, it can be handy to use `grunt watch` to run a watcher daemon that re-compiles
LESS or JS files whenever a change was made in the respective [resources](resources/) directory.

### resources/structure.json.in ###

This configuration file can be used to define the website structure.
The file consists at its root of an array, which will be transformed into the main menu of the website
(the dynamic part of the menu).
The array houses objects, of which every object represents an element on the website (e.g. a link).

In the following, the different element types will be explained in detail.
The headline always refers to the `type` field of the element.
The element type `link` for example would be an object like the following
with some extra attributes explained below:

    {
        "type": "link",
        ... other attributes ...
    }

It is possible to add additional attributes not used by the system without breaking anything.
For example use `dev-comment` to leave some development notes, e.g. decision information.

#### submenu ####

The `submenu` type can be used to create a menu point that has a (hoverable) submenu,
but does itself not link to any page. It can only be used in the top hierarchy of the structure file.

This field type supports following attributes:
- `name` (string) for the visible name of the menu point (i.e. button text)
- `ref` (string) for the dynamic URL part (i.e. a resource of the URL,
  e.g. `http://example.com/docs` for the subsequent example)
- `children` (array) holding other structure elements, but none of type `submenu`

Example:

    {
        "name": "Documentation",
        "type": "submenu",
        "ref": "docs",
        "children": [ ]
    }

#### parsereadme ####

The `parsereadme` element type is the most powerful of all types.
It takes a text file as input (often README.md) and creates with the help of some
regex patterns a section of the website which contains parsed links of the input file.

This field type support following attributes:
- `name` (string) for the visible name of the menu point (i.e. button text)
- `ref` (string) for the dynamic URL part (i.e. a resource of the URL,
  e.g. `http://example.com/plugins` for the subsequent example)
- `options` (object) with further options:
    - `path` (string) containing the path from the repository root to the text file to parse
    - `target_file` (array[string]) containing some filenames that should be
      targeted for parsed links that are no files (i.e. links to directories)
    - `parsing` (object) with further options:
        - `start_regex` (string, optional) defines the start point from where on
          the following regex types should be parsed
        - `entry_regex` (string) defines a regex that will create links to
          files within a website section
        - `section_regex` (string, optional) can additionally be used to
          parse group names which will make the section links look nicer
        - `stop_regex` (string, optional) defines the end point up to
          which the text file will be parsed
    - `name` (object) with further options:
        - `make_pretty` (boolean) whether the link names within the
          text file which will also be used on the website should be made pretty
          (e.g. first-capitalize, etc.);
          this option is discouraged for this structure element type

Example:

    {
        "name": "Plugins",
        "type": "parsereadme",
        "ref": "plugins",
        "options": {
            "path": "src/plugins/README.md",
            "target_file": ["README.md", "README", "readme.md", "readme"],
            "parsing": {
                "start_regex": "# Plugins #",
                "stop_regex": "####### UNUSED ######",
                "section_regex": "### ([^#]+) ###",
                "entry_regex": "^\\- \\[(.+)\\]\\(([^\\)]+)\\)(.*)"
            },
            "name": {
                "make_pretty": false
            }
        }
    }

#### listdirs ####

The `listdirs` element type can be used to enumerate all sub-directories of a specific directory.
It will try to find one of the target files (i.e. readme) within the sub-directories and
create a link to them. All this is done in a newly created website section.

This field type supports following attributes:
- `name` (string) for the visible name of the menu point (i.e. button text)
- `ref` (string) for the dynamic URL part (i.e. a resource of the URL,
  e.g. `http://example.com/tools` for the subsequent example)
- `options` (object) with further options:
    - `path` (string) containing the path from the repository root to the directory to enumerate
    - `target_file` (array[string]) containing some filenames that should be targeted within the sub-directories 
      (e.g. find file `README.md` in directory `mydir` to use it as information file for the directory)

Example:

    {
        "name": "Tools",
        "type": "listdirs",
        "ref": "tools",
        "options": {
            "path": "src/tools",
            "target_file": ["README.md", "README", "readme.md", "readme"]
        }
    }

#### listfiles ####

The `listfiles` element type is quite similar to the `listdirs` type,
but instead of sub-directories it enumerates files within a directory.
It does also create a new website section.

This field type supports following attributes:
- `name` (string) for the visible name of the menu point (i.e. button text)
- `ref` (string) for the dynamic URL part (i.e. a resource of the URL,
  e.g. `http://example.com/manpages` for the subsequent example)
- `options` (object) with further options:
    - `path` (string) containing the path from the repository root to the directory to enumerate
    - `blacklist` (array[string]) containing some filenames that should
      be excluded from the result (e.g. CMakeLists.txt)

Example:

    {
        "name": "Manpages",
        "type": "listfiles",
        "ref": "manpages",
        "options": {
            "path": "doc/help",
            "blacklist": ["CMakeLists.txt"]
        }
    }

#### staticlist ####

The `staticlist` element type creates a new website section that is
entirely customizable within the structure configuration file.
This type can be used instead of the `parsereadme` type if a mix of many types is required.

This field type supports following attributes:
- `name` (string) for the visible name of the menu point (i.e. button text)
- `ref` (string) for the dynamic URL part (i.e. a resource of the URL,
  e.g. `http://example.com/getstarted` for the subsequent example)
- `children` (array) holding static structure elements like `staticref`, `staticfile` and `link`

Example:

    {
        "name": "Getting started",
        "type": "staticlist",
        "ref": "getstarted",
        "children": [ ]
    }

#### staticref ####

The `staticref` element type can be used in a `staticlist` to create a reference to another website part.

This field type support following attributes:
- `name` (string) for the visible name of the menu point (i.e. button text)
- `options` (object) with further options:
    - `path` (string) containing a reference, which can either be the `ref`
      attribute of another element or an even more specific reference

Example:

    {
        "name": "Tutorials",
        "type": "staticref",
        "options": {
            "path": "tutorials"
        }
    }

#### staticfile ####

The `staticfile` element type can be used in a `staticlist` to create a menu point for a file.
The file is then a page in the section created by the `staticlist`.

This field type support following attributes:
- `name` (string) for the visible name of the menu point (i.e. button text)
- `options` (object) with further options:
    - `path` (string) containing the path to a file

Example:

    {
        "name": "Installation",
        "type": "staticfile",
        "options": {
            "path": "doc/INSTALL.md"
        }
    }

#### link ####

The `link` element type can be used to create a simple link to whatever is desired.
It is recommended to use it only for external links.

This field type support following attributes:
- `name` (string) for the visible name of the menu point (i.e. button text)
- `ref` (string) for the dynamic URL part (_currently unused_)
- `options` (object) with further options:
    - `path` (string) containing the path of the link

Example:

    {
        "name": "Build Server",
        "type": "link",
        "ref": "buildserver",
        "options": {
            "path": "https://build.libelektra.org/"
        }
    }

## Development ##

When attempting to change the AngularJS application, it can be useful to first
have a look at all used dependencies, which are listed in
[resources/assets/js/application.js](resources/assets/js/application.js).
After that, the configuration files in
[resources/assets/js/config](resources/assets/js/config) should be checked.
Probably the most important configuration is the router in
[resources/assets/js/config/routes.config.js](resources/assets/js/config/routes.config.js).

### Life Cycle ###

An AngularJS application is bootstrapped by first instantiating constants (can be used for configuration).
After that, service providers are run, which allows for further configuration of services.
When the bootstrap process is finished and all services are instantiated based
on the settings made within the service providers, the router will load
the default route (main page) and bind the appropriate controller to it.
Controllers are destroyed as soon as a page is changed, but services are not.
So caching across pages can be done using services.
AngularJS also allows for dependency injection in basically every part of the
application (services, controllers, etc) by type-hinting the dependency name.

For detailed information, the website of [Angular](https://angularjs.org/) should be visited.

### Task configuration ###

All `grunt` tasks can be configured using the [Gruntfile.js](Gruntfile.js) in the application root directory.

### Code formatting ###

The task `grunt jshint` can be used to check the code formatting of JS source files.

### Noteworthy Information ###

#### HTML in i18n Keys ####

It is possible to use HTML in translation files (loca keys) if the place where
the loca key is used adds the directive `translate-compile`. The loca key itself
does also need to be placed in the `translate` directive instead of a dynamic
Angular binding (i.e. use `<span translate="LOCA_KEY"></span>` in favor of
`<span>{{ 'LOCA_KEY' | translate }}</span>`).

#### Links ####

For external links, the normal HTML `a`-tag has to be used (`<a href="..."></a>`).
If the external link has the same base URL as the frontend (e.g. frontend is at
`http://localhost/` and the link points to `http://localhost/news/feed.rss`),
the html tag `target` has to be added to the link with the desired value, e.g.
`_self` to open the link in the same window/tab or `_blank` to use a new one.
An example would be `<a href="http://localhost/news/feed.rss" target="_self">...</a>`.

For internal links (that are links that lead to another sub-page of the website)
two options are available. It is possible to use the normal `href` HTML attribute
or to use the special `ui-sref` attribute defined by the frontend router.
The `ui-sref` directive works on state names and not on links,
so if a sub-page like `<website-url>/docs/tutorials` exists, one cannot use
`<a ui-sref="/docs/tutorials">...</a>`; the state name for the tutorials page has to
be used, which is most likely `main.dyn.tutorials` if the tutorials section is based
on the `structure.json.in`. The link (with a simple loca key) would look like
`<a ui-sref="main.dyn.tutorials">...</a>` therefore.
The `ui-sref` variant requires the HTML to be specially compiled though, what makes
the usage of a normal `href` attribute easier in most scenarios.
The following link does exactly the same as the last example with `ui-sref`:
`<a href="/docs/tutorials">...</a>`.
An advantage of `ui-sref` over `href` is that it does also work with hidden parameters,
i.e. state parameters not visible in the URL. Such parameters are rarely used in
practice, though, as they are not SEO friendly at all.
