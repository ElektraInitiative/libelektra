# Technical description

## REST service

The REST service will be implemented using [CppCMS](http://cppcms.com/wikipp/en/page/main). A detailed description of the implemented API can be found in the [API description](api-description.apib). The REST service will leave some space for configuration, as not every deployment will look the same.

## Front-end

The front-end will be implemented using AngularJS (v1.5), i.e. a SPA (Single Page Application) will be developed. The final SPA will only consist of static files (.html, .js, .css, .jpg, ...), which means that it could be deployed within any web server. To build the SPA from the development resources, [Grunt](http://gruntjs.com/) is used as task runner.

### Elektra Website

One part of the front-end will be the new Elektra website containing documentation, tutorials and other important artifacts. If it is possible to display the documentation artifacts without changing their current arrangement (i.e. rename them on GitHub), it would also be easily possible to implement per-version documentation.

Resources will be fetched directly from GitHub. It is planned to load all content of the website from GitHub, to avoid having to change the front-end itself. The front-end will support Markdown syntax via a plugin like [Showdown](https://github.com/showdownjs/showdown). Styling can then be done easily through CSS.

#### Example

An example how the website could look like is the [Laravel website](https://laravel.com/). On the main page the basic idea is presented, whereas the main focus lies on the documentation, which is simply beautiful.

#### Global search

It is not planned to implement a global search for the website itself (documentation, tutorials, ...), as the resources come directly from GitHub (but from multiple endpoints). The available search will only search the configuration snippet database.

#### Website structure

TBD
