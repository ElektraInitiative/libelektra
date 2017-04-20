# Implementation details & Technical description #

## REST service (Backend) ##

The REST service will be implemented using [CppCMS](http://cppcms.com/wikipp/en/page/main). A detailed description of the implemented API can be found in the [API description](/doc/api_blueprints/snippet-sharing.apib). The REST service will leave some space for configuration, as not every deployment will look the same.

## Front-end ##

The front-end will be implemented using AngularJS (v1.5), i.e. a SPA (Single Page Application) will be developed. The final SPA will only consist of static files (.html, .js, .css, .jpg, ...), which means that it could be deployed within any web server. To build the SPA from the development resources, [Grunt](http://gruntjs.com/) is used as task runner and [npm](https://www.npmjs.com/) as dependency manager.

### Snippet Sharing ###

Sharing of snippets will require authentication, i.e. registration and login will be implemented. Snippets can then be looked up by using the search function, which will offer some convenience options like filter and sorting. Snippets themselves can then be viewed in any supported format, downloaded, copied, etc.

Besides the snippet sharing functionality, the whole front-end implements a basic user system along with a permission system, allowing for higher roles with more privileges.

### Elektra Website ###

One part of the front-end will be the new Elektra website containing documentation, tutorials and other important artifacts like news. Almost all necessary resources will be generated and copied from a local repository clone to the website deployment during build (to refresh the website, a new build is necessary). 

#### Important facts ####

- Links are internal on the website if the target is part of it too, otherwise they are external (i.e. linked to repo on external site).
- The front-end will support Markdown syntax via a plugin like [Marked](https://github.com/chjj/marked).
- Styling can then be done easily through LESS/CSS.
- The website structure will be dynamically adjustable. There will be a set of types which can be used to define links, menus, content, etc. A detailed discussion for the website structure happens in #1015.

#### Example reference ####

An example how the website could look like is the [Laravel website](https://laravel.com/). On the main page the basic idea is presented, whereas the main focus lies on the documentation, which is simply beautiful.

#### Limitations ####

**Global search:**
It is not planned to implement a global search for the website itself (documentation, tutorials, ...) as the resources are not easily searchable (static files, everything happens in the browser, i.e. pre-fetch of all files would be necessary for a search). The available search will only search the configuration snippet database (and users, for admins).
