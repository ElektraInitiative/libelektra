Plugins can be mounted into the KDB and can access or manipulate the
KeySet.

# Introduction #

Elektra already has a wide range of different plugins.
The plugin folders should contain a README.md with further information.
The plugins are:

![Overview Plugins](/doc/images/overview_plugins.png)


## Resolver ##

Before configuration is actually written, the file name needs to be
determined (will be automatically added by kdb mount):

- [resolver](resolver) uses POSIX APIs to handle conflicts gracefully
- [wresolver](wresolver) minimalistic resolver for non-POSIX systems
- [noresolver](noresolver) does not resolve, but can act as one

and afterwards the configuration file must be synced with
harddisc (recommended to add at every kdb mount):

- [sync](sync) uses POSIX APIs to sync configuration file with harddisc

## Storage ##

Are responsible for reading writing the configuration to configuration
files.

Read and write everything a KeySet might contain:

- [dump](dump) makes a dump of a KeySet in an Elektra-specific format

Read (and write) standard config files of /etc:

- [augeas](augeas) parses and generates many different configuration
  files using the augeas library
- [hosts](hosts) read/write hosts files
- [line](line) reads any file line by line

Using semi-structured data for config files:

- [tcl](tcl)-like config files (including meta data).
- [ni](ni) parses INI files based on
    [ni](https://github.com/chazomaticus/bohr/blob/master/include/bohr/ni.h).
- [ini](ini) parses INI files based on
    [inih](http://code.google.com/p/inih/).
- [xmltool](xmltool) uses XML.
- [yajl](yajl#introduction) uses JSON.

Plugins that just show some functionality, (currently) not intended for
productive use:

- [fstab](fstab) reads fstab files.
- [regexstore](regexstore)
- [simpleini](simpleini) is ini without sections
- [csvstorage](csvstorage) for csv files

## System Information ##

Information compiled in Elektra:
- [version](version) is a special-buildin plugin directly within the
  core so that it cannot give wrong version information
- [constants](constants) various constants, including version
  information

Providing information found on the system not available in persistent
files:

- [uname](uname) information from the uname syscall.


## Filter ##

Rewrite unwanted characters with different techniques

- [ccode](ccode) using the technique from arrays in the programming
  language C
- [hexcode](hexcode) using hex codes

Transformations:

- [keytometa](keytometa) transforms keys to metadata
- [rename](rename) renames keys according to different rules

Doing other stuff:

- [iconv](iconv) make sure the configuration will have correct
  charachter encoding
- [hidden](hidden) 
- [null](null) takes care of null values and other binary specialities


## Notification and Logging ##

Log/Send out all changes to configuration to:

- [dbus](dbus)
- [journald](journald)
- [syslog](syslog)
- [logger](logger) logs errors, warnings and other informations to a file

## Debug ##

Trace everything that happens within KDB:

- [timeofday](timeofday) print timestamps
- [tracer](tracer)
- [counter](counter) count and print how often plugin is used


## Checker ##

Copies meta data to keys:

- [glob](glob) using globbing techniques
- [struct](struct) using a defined structure (may also reject
  configuration not conforming to that structure)
- [globalglob](globalglob) copies from spec to other namespaces using globbing techniques and does a structure check

Plugins that check if values are valid based on meta data (typically
copied by another plugin just before):

- [validation](validation) by using regex
- [network](network) by using network APIs
- [path](path) by checking files on filesystem
- [type](type) using runtime type checking (CORBA types)
- [enum](enum) compares the keyvalue against a list of valid values

## Interpreter ##

These plugins start an interpreter and allow you to use a bindings.

- [jni](jni) java plugins started by jni, works with jna plugins
- [python](python) Python 3 plugins (technical preview)
- [python2](python2) Python 2 plugins (technical preview, deprecated)


## Others ##

- [doc](doc) contains the documentation of the plugin interface
- [error](error) yields errors as described in metadata
- [template](template) to be copied for new plugins
- [lineendings](lineendings) tests file for consistent line endings
- [list](list) loads other plugins

To add a new plugin you can copy the template plugin. Please make sure
to add your plugin:

- to this list, at least in Others
- to [cmake](/cmake/ElektraCache.cmake), at least in ALL
