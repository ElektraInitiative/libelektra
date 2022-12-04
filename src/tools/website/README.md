# Elektra's Website

## Introduction

This document aims to provide information about how to build Elektra’s Website.

## Design and Structure

The website is developed as a single-page application (SPA) in [AngularJS (v1.5)](https://angularjs.org/).
All dependencies are either already contained in the application project or (preferred) resolved through
the dependency manager [npm](https://www.npmjs.com/) during installation (requires active internet connection).
Compiling (browserification, concatenation & minification), as well as other tasks like running a lightweight
webserver are handled by the nodeJS based task runner [grunt](http://gruntjs.com/) (installed by npm).

## Run and Configure

To install the website, make sure `website` is included in TOOLS. You can use [this guide](https://github.com/ElektraInitiative/libelektra/blob/master/doc/COMPILE.md) to add the required website tool.
`npm` is the only dependency.
Then use `make install` to install the website.
It will be installed in `@CMAKE_INSTALL_PREFIX@/share/elektra/tool_data/website/public`.
Default values for `CMAKE_INSTALL_PREFIX` can be found [here](https://cmake.org/cmake/help/latest/variable/CMAKE_INSTALL_PREFIX.html).

As next step, the website configuration needs to be copied and mounted:
`@CMAKE_INSTALL_PREFIX@/lib/elektra/tool_exec/mount-website-config`

The configuration will be mounted to `system:/sw/elektra/website/#0/current` in Elektra
(`@config_root@/@config_default_profile@`)
and contains some URLs for GitHub resources and translation,
as well as logger settings. Usually, no changes are required there, see
Configuration Options below for some useful options.

As next step, you can build the website:
`@CMAKE_INSTALL_PREFIX@/lib/elektra/tool_exec/build-website` (or `kdb build-@tool@`)

To run the application, basically two options are available:

- Use the built-in webserver of `grunt`, which can be configured in the
  [Gruntfile.js](Gruntfile.js) and run by `grunt server` (in the installation target directory `@CMAKE_INSTALL_PREFIX@/share/elektra/tool_data/website`)
  or `kdb run-@tool@` from anywhere.
  To stop the `@tool@`, run `kdb stop-@tool@`.
- Use an own webserver to distribute the application.
  In order to do so, only `grunt full` (or `kdb build-@tool@`, see above) needs to be run.
  After that, the content of the [public](public/) directory can be copied to any location
  that suits the needs. `npm` dependencies in the [node_modules](node_modules/) directory
  and the [resources](resources/) directory are only necessary for development,
  but can be ignored for deployment. The required dependencies were copied to the [public](public/)
  directory already.

In order to not receive any 404 errors by the webserver, it should serve the `index.html`
for all requests that do not have a static file as target.
The `index.html` will then try to serve the (dynamic) URL itself.

### Configuration Options

#### PID file

Using ${config_root}${config_default_profile}/daemon/lock (i.e., `daemon.lock` in JSON) you can specify which PID file should be used.
Default: /run/elektra-@tool@.pid

#### APIs (GitHub)

The configuration file allows to change GitHub settings in the `github` section.

#### Translations

The configuration file also allows to specify available translations in `translations.enabled`.
To add a translation, copy an existing translation file in
[public/assets/translations](public/assets/translations), translate it and add the name of
the new language to the list in `translations.enabled`.
After that run `grunt full` (or `kdb build-@tool@`) to re-compile the application.

If necessary, mappings for dialects as well as a default language can be specified as well.

#### Logger

It is possible to enable the frontend logger by changing `logger.enabled` in the configuration file.

### Directory Structure

The application project itself is mainly splitted into two directories: `resources` and `public`,
whereas only the latter can directly been accessed by clients if the built-in
`grunt server` is used to deploy the project.

The [resources](resources/) directory contains the JavaScript source files,
custom grunt tasks as well as the LESS files which are compiled into CSS files for the website.

The [public](public/) directory contains HTML template files, assets like fonts,
compiled JS and CSS files, as well as translation files and all dependencies resolved by `npm`,
which are copied by `grunt`. It does also contain copied documentation files for the website.

### Important facts

- Links are internal on the website if the target is part of it too,
  otherwise they are external (i.e. linked to repo on external site).
- The frontend supports Markdown syntax via a plugin ([marked](https://github.com/chjj/marked)).
- Styling can then be done easily through LESS/CSS.
- The website structure is dynamically adjustable.
  There is a set of types which can be used to define links, menus, content of sites, etc.
  A detailed discussion for the website structure happened in #1015.
- A full text search using Algolia is implemented https://issues.libelektra.org/2796

### Limitations

**AngularJS 2:**
At the time of development start, there was no stable AngularJS 2 release available yet,
only early previews. Because of that, the frontend was developed using AngularJS 1.5.
Later we tried an upgrade but failed of the extensive work which would be required.
Another idea would be to build a static webpage, so that at least the end user
is not confronted with the old version of AngularJS: https://issues.libelektra.org/3470

## Compiling and Installing

### Dependencies

The project has quite a few dependencies, of which most can be resolved automatically
by the used package manager.
The only dependency that has to be installed beforehand is the package manager
[npm](https://www.npmjs.com/) itself,
which comes bundled with [Node.js](https://nodejs.org/) (preferred installation).

### Compiling

The `@tool@` has full CMake integration, which does actually only two things:

- Install (copy) the project files to a target directory.
- Run `npm install` in this target directory, which does
- resolve all `npm` dependencies (into the directory [node_modules](node_modules/)).
- run `grunt full` to compile all application sources ([resources](resources/) dir)
  into working production files ([public](public/) dir) and
  copy required `npm` dependencies in the `public` folder.

### Installing

It is not necessary to install anything by hand, CMake does this job already.
If changes are made to the source files in [resources](resources/),
it is sufficient to run `grunt full` (or `kdb build-@tool@`) to build the application again.
During development, it can be handy to use `grunt watch` to run a watcher daemon that re-compiles
LESS or JS files whenever a change was made in the respective [resources](resources/) directory.

### resources/structure.json.in

This configuration file can be used to define the website structure.
The file consists at its root of an array, which will be transformed into the main menu of the website
(the dynamic part of the menu).
The array houses objects, of which every object represents an element on the website (e.g. a link).

In the following, the different element types will be explained in detail.
The headline always refers to the `type` field of the element.
The element type `link` for example would be an object like the following
with some extra attributes explained below:

```
{
    "type": "link",
    ... other attributes ...
}
```

It is possible to add additional attributes not used by the system without breaking anything.
For example use `dev-comment` to leave some development notes, e.g. decision information.

#### submenu

The `submenu` type can be used to create a menu point that has a (hoverable) submenu,
but does itself not link to any page. It can only be used in the top hierarchy of the structure file.

This field type supports following attributes:

- `name` (string) for the visible name of the menu point (i.e. button text)
- `ref` (string) for the dynamic URL part (i.e. a resource of the URL,
  e.g. `http://example.com/docs` for the subsequent example)
- `children` (array) holding other structure elements, but none of type `submenu`

Example:

```json
{
  "name": "Documentation",
  "type": "submenu",
  "ref": "docs",
  "children": []
}
```

#### parsereadme

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

```json
{
  "name": "Plugins",
  "type": "parsereadme",
  "ref": "plugins",
  "options": {
    "path": "src/plugins/README.md",
    "target_file": ["README.md", "README", "readme.md", "readme"],
    "parsing": {
      "start_regex": "# Plugins",
      "stop_regex": "####### UNUSED",
      "section_regex": "### ([^#]+)",
      "entry_regex": "^\\- \\[(.+)\\]\\(([^\\)]+)\\)(.*)"
    },
    "name": {
      "make_pretty": false
    }
  }
}
```

#### listdirs

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

```json
{
  "name": "Tools",
  "type": "listdirs",
  "ref": "tools",
  "options": {
    "path": "src/tools",
    "target_file": ["README.md", "README", "readme.md", "readme"]
  }
}
```

#### listfiles

The `listfiles` element type is quite similar to the `listdirs` type,
but instead of sub-directories it enumerates files within a directory.
It does also create a new website section.

This field type supports following attributes:

- `name` (string) for the visible name of the menu point (i.e. button text)
- `ref` (string) for the dynamic URL part (i.e. a resource of the URL,
  e.g. `http://example.com/man-pages` for the subsequent example)
- `options` (object) with further options:
  - `path` (string) containing the path from the repository root to the directory to enumerate
  - `blacklist` (array[string]) containing some filenames that should
    be excluded from the result (e.g. CMakeLists.txt)

Example:

```json
{
  "name": "Man pages",
  "type": "listfiles",
  "ref": "man-pages",
  "options": {
    "path": "doc/help",
    "blacklist": ["CMakeLists.txt"]
  }
}
```

#### staticlist

The `staticlist` element type creates a new website section that is
entirely customizable within the structure configuration file.
This type can be used instead of the `parsereadme` type if a mix of many types is required.

This field type supports following attributes:

- `name` (string) for the visible name of the menu point (i.e. button text)
- `ref` (string) for the dynamic URL part (i.e. a resource of the URL,
  e.g. `http://example.com/getstarted` for the subsequent example)
- `children` (array) holding static structure elements like `staticref`, `staticfile` and `link`

Example:

```json
{
  "name": "Getting started",
  "type": "staticlist",
  "ref": "getstarted",
  "children": []
}
```

#### staticref

The `staticref` element type can be used in a `staticlist` to create a reference to another website part.

This field type support following attributes:

- `name` (string) for the visible name of the menu point (i.e. button text)
- `options` (object) with further options:
  - `path` (string) containing a reference, which can either be the `ref`
    attribute of another element or an even more specific reference

Example:

```json
{
  "name": "Tutorials",
  "type": "staticref",
  "options": {
    "path": "tutorials"
  }
}
```

#### staticfile

The `staticfile` element type can be used in a `staticlist` to create a menu point for a file.
The file is then a page in the section created by the `staticlist`.

This field type support following attributes:

- `name` (string) for the visible name of the menu point (i.e. button text)
- `options` (object) with further options:
  - `path` (string) containing the path to a file

Example:

```json
{
  "name": "Installation",
  "type": "staticfile",
  "options": {
    "path": "doc/INSTALL.md"
  }
}
```

#### link

The `link` element type can be used to create a simple link to whatever is desired.
It is recommended to use it only for external links.

This field type support following attributes:

- `name` (string) for the visible name of the menu point (i.e. button text)
- `ref` (string) for the dynamic URL part (_currently unused_)
- `options` (object) with further options:
  - `path` (string) containing the path of the link

Example:

```json
{
  "name": "Build Server",
  "type": "link",
  "ref": "buildserver",
  "options": {
    "path": "https://build.libelektra.org/"
  }
}
```

#### parsefolders

The `parsefolders` element type looks at list of folders and creates table of contents (TOC) file for the contents.
For every folder a section is appended to the TOC file.
The section will contain a list linking to all the files in the folder.

This field type support following attributes:

- `name` (string) for the visible name of the menu point (i.e. button text)
- `ref` (string) for the dynamic URL part (_currently unused_)
- `options` (object) with further options:
  - `path` (string) containing the path of the base folder
  - `base_toc` (string) filename of the base file for generating the TOC
  - `folders` (array of objects) list of folder to traverse
    - `path` (string) path of folder relative to base folder
    - `title` (string) title for the section of this folder
    - `title_level` (number) level of the title (adds this number of `#` before the title to create a Markdown title)

Example:

```json
{
  "name": "Decisions",
  "type": "parsefolders",
  "ref": "decisions",
  "options": {
    "path": "doc/decisions",
    "base_toc": "README.md",
    "folders": [
      {
        "path": "0_drafts",
        "title": "Drafts",
        "title_level": 2
      }
    ]
  }
}
```

## Development

When attempting to change the AngularJS application, it can be useful to first
have a look at all used dependencies, which are listed in
[resources/assets/js/application.js](resources/assets/js/application.js).
After that, the configuration files in
[resources/assets/js/config](resources/assets/js/config) should be checked.
Probably the most important configuration is the router in
[resources/assets/js/config/routes.config.js](resources/assets/js/config/routes.config.js).

### Life Cycle

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

### Task Configuration

All `grunt` tasks can be configured using the [Gruntfile.js](Gruntfile.js) in the application root directory.

### Code Formatting

The task `grunt jshint` can be used to check the code formatting of JS source files.

### Noteworthy Information

#### HTML in i18n Keys

It is possible to use HTML in translation files (loca keys) if the place where
the loca key is used adds the directive `translate-compile`. The loca key itself
does also need to be placed in the `translate` directive instead of a dynamic
Angular binding (i.e. use `<span translate="LOCA_KEY"></span>` in favor of
`<span>{{ 'LOCA_KEY' | translate }}</span>`).

#### Links

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
