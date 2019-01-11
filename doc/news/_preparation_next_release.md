# 0.8.<<VERSION>> Release

This release did not happen yet.

Please update this file within PRs accordingly.
For non-trivial changes, you can choose to be
part of the highlighted changes. Please make
sure to add some short tutorial, asciinema,
or how-to-use for highlighted items.

Please add your name to every contribution
syntax: ", thanks to <myname>".


<<`scripts/generate-news-entry`>>

We are proud to release Elektra 0.8.<<VERSION>>.

## What is Elektra?

Elektra serves as a universal and secure framework to access
configuration settings in a global, hierarchical key database.
For more information, visit [https://libelektra.org](https://libelektra.org).

For a small demo see here:

[![asciicast](https://asciinema.org/a/cantr04assr4jkv8v34uz9b8r.png)](https://asciinema.org/a/cantr04assr4jkv8v34uz9b8r)

You can also read the news [on our website](https://www.libelektra.org/news/0.8.<<VERSION>>-release)



## Highlights

- The new High-Level-API has been added. *(Klemens Böswirth)*
- The new High-Level-API has been added. *(Klemens Böswirth)*
- <<HIGHLIGHT2>>
- <<HIGHLIGHT3>>

### High-Level API
The new high-level API provides an easier way to get started with Elektra.

To get started (including proper error handling) you now only need a few self-explanatory lines of code:
```c
ElektraError * error;
Elektra * elektra = elektraOpen ("/sw/org/myapp/#0/current", NULL, &error);
if (elektra == NULL)
{
	printf ("An error occured: %s", elektraErrorDescription (error));
	elektraErrorReset (error);
	return -1;
}

### High-Level API
The new high-level API provides an easier way to get started with Elektra.

To get started (including proper error handling) you now only need a few self-explanatory lines of code:
```c
ElektraError * error;
Elektra * elektra = elektraOpen ("/sw/org/myapp/#0/current", NULL, &error);
if (elektra == NULL)
{
	printf ("An error occured: %s", elektraErrorDescription (error));
	elektraErrorReset (error);
	return -1;
}

// use API

elektraClose (elektra);
```

Once you have an instance of `Elektra` you simply call one of the typed `elektraGet*` functions to read a value:
```c
const char * mystring = elektraGetString (elektra, "mystring");
```
No need to specify the base path `/sw/org/myapp/#0/current` anymore, as the high-level API keeps track of that for you.
The API supports the CORBA types already used by some plugins. The high-level API should also be used in combination
with a specification (`spec-mount`). When used this way, the API is designed to be error and crash free while reading values.
Writing values, can of course still produces errors.

Another advantage of the new API is, that it will be much easier to write bindings for other languages now, because only a few simply
types and functions have to be mapped to provide the full functionality.

Take a look at the [README](/src/libs/highlevel/README.md) for more infos.


### <<HIGHLIGHT2>>


### <<HIGHLIGHT2>>


## Plugins

The following section lists news about the [modules](https://www.libelektra.org/plugins/readme) we updated in this release.

### <<Plugin1>>

- <<TODO>>
- <<TODO>>
- <<TODO>>



### <<Plugin2>>

- <<TODO>>
- <<TODO>>
- <<TODO>>


  now works correctly. *(René Schwaiger)*

### path

Enhanced the plugin to also check for concrete file or directory permissions such as `rwx`. *(Michael Zronek)*

### YAwn

This new plugin parses a subset of YAML using the Earley Parser library [YAEP](https://github.com/vnmakarov/yaep). *(René Schwaiger)*

### <<Plugin3>>

- <<TODO>>
- <<TODO>>
- <<TODO>>


## Libraries

The text below summarizes updates to the [C (and C++)-based libraries](https://www.libelektra.org/libraries/readme) of Elektra.


### Compatibility

As always, the ABI and API of kdb.h is fully compatible, i.e. programs
compiled against an older 0.8 version of Elektra will continue to work
(ABI) and you will be able to recompile programs without errors (API).

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Core

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Libease

- The function `elektraArrayValidateBaseNameString` now returns the offset to the first digit of the array index, if the given string
  represents an array element containing an index. This update enhances the behavior of the function. Now it not only tells you if a name
  represents a valid array element, but also the start position of the array index.

  ```c
  elektraArrayValidateBaseNameString ("#_10");
  //                                     ~~^ Returns `2` (instead of `1`)

  elektraArrayValidateBaseNameString ("#___1337");
  //                                   ~~~~^ Returns `4` (instead of `1`)
  ```

  If your program already used `elektraArrayValidateBaseNameString` and you check for a valid array element using the equality operator
  (`== 1`), then please use (`>= 1`) instead. For example, if you code that looks like this:

  ```c
  if (elektraArrayValidateBaseNameString(baseName) == 1) …;
  ```

  , please update your code to check for a valid array element name like this:

  ```c
  if (elektraArrayValidateBaseNameString(baseName) >= 1) …;
  ```

  . *(René Schwaiger)*
- <<TODO>>
- <<TODO>>


### <<Library2>>

- <<TODO>>
- <<TODO>>
- <<TODO>>


### <<Library3>>

- <<TODO>>
- <<TODO>>
- <<TODO>>


## Bindings

Bindings allow you to utilize Elektra using [various programming languages](https://www.libelektra.org/bindings/readme). This section keeps
you up to date with the multi-language support provided by Elektra.

### <<Binding1>>


### <<Binding2>>


### <<Binding3>>


## Tools

- <<TODO>>
- <<TODO>>
- <<TODO>>


## Scripts

- <<TODO>>
- <<TODO>>
- <<TODO>>


## Documentation

- We fixed various spelling mistakes. *(René Schwaiger)*
- <<TODO>>
- <<TODO>>

## Tests

- The tests for the IO bindings and notification plugins now use increased timeout values to make sure the test suite fails less often on
  machines with high load. *(René Schwaiger)*
- <<TODO>>
- <<TODO>>


## Build

### CMake

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Docker

- <<TODO>>
- <<TODO>>
- <<TODO>>


## Infrastructure

### Jenkins

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Travis

- We now test Elektra on [Ubuntu Xenial Xerus](https://docs.travis-ci.com/user/reference/xenial). *(René Schwaiger)*
- <<TODO>>
- <<TODO>>


## Website

The website is generated from the repository, so all information about
plugins, bindings and tools are always up to date. Furthermore, we changed:

- <<TODO>>
- <<TODO>>
- <<TODO>>


## Outlook

We are currently working on following topics:

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Statistics

Following authors made this release possible:

<<`scripts/git-release-stats 0.8.<<VERSION>>`>>

We welcome new contributors!


## Get It!

You can download the release from [here](https://www.libelektra.org/ftp/elektra/releases/elektra-0.8.<<VERSION>>.tar.gz)
or [GitHub](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.8.<<VERSION>>.tar.gz?raw=true)


The [hashsums are:](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.8.<<VERSION>>.tar.gz.hashsum?raw=true)

<<`scripts/generate-hashsums`>>

The release tarball is also available signed by Markus Raab using GnuPG from
[here](https://www.libelektra.org/ftp/elektra/releases/elektra-0.8.<<VERSION>>.tar.gz.gpg) or on
[GitHub](https://github.com/ElektraInitiative/ftp/blob/master/releases//elektra-0.8.<<VERSION>>.tar.gz.gpg?raw=true)

Already built API-Docu can be found [here](https://doc.libelektra.org/api/0.8.<<VERSION>>/html/)
or on [GitHub](https://github.com/ElektraInitiative/doc/tree/master/api/0.8.<<VERSION>>).


## Stay tuned!

Subscribe to the
[RSS feed](https://www.libelektra.org/news/feed.rss)
to always get the release notifications.

If you also want to participate, or for any questions and comments
please contact us via the issue tracker [on GitHub](http://issues.libelektra.org).

[Permalink to this NEWS entry](https://www.libelektra.org/news/0.8.<<VERSION>>-release)

For more information, see [https://libelektra.org](https://libelektra.org)

Best regards,
[Elektra Initiative](https://www.libelektra.org/developers/authors)


