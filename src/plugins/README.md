elektra-plugins(7) -- plugins overview
======================================

Plugins can be mounted into the KDB and can access or manipulate the
KeySet on every access.

Multiple plugins can be mounted into the [key data base](/doc/help/elektra-glossary.md).
On every access to the key data base they are executed and thus can change
the functionality.



# Introduction #

Elektra already has a wide range of different plugins.
The plugin folders should contain a README.md with further information.
(Or follow links below.)
The plugins are:

![Overview Plugins](/doc/images/overview_plugins.png)

For background information see [elektra-plugins-framework(7)](/doc/help/elektra-plugins-framework.md).


## C-Interface ##

All plugins implement the same interface:

-  `kdbOpen()` calls `elektraPluginOpen()` of every plugin
    to let them do their initialisation.
-  `kdbGet()` requests `elektraPluginGet()` of every plugin in the queried
    backends to return a key set.
-  `kdbSet()` usually calls `elektraPluginSet()` of every plugin
    in the queried backends to store the configuration.
-  `kdbSet()` also calls `elektraPluginError()`
    for every plugin when an error happens.
    Because of `elektraPluginError()`, plugins are guaranteed to have
    their chance for necessary cleanups.
-  `kdbClose()` makes sure that plugins can finally free their
    own resources in `elektraPluginClose()`.


## KDB-Interface

- To list all plugins use [kdb-list(1)](/doc/help/kdb-list.md).
- To check a plugin use [kdb-check(1)](/doc/help/kdb-check.md).
- For information on a plugin use [kdb-info(1)](/doc/help/kdb-info.md).
- For mount plugin(s) use [kdb-mount(1)](/doc/help/kdb-mount.md).


## See also

For an easy introduction, see [this tutorial how to write a storage plugin](/doc/tutorials/plugins.md).
For more background information of the [plugins framework, continue here](/doc/help/elektra-plugins-framework.md).
Otherwise, you can visit the [the API documentation](http://doc.libelektra.org/api/current/html/group__plugin.html).




# Plugins #

## Resolver ##

Before configuration is actually written, the file name needs to be
determined (will be automatically added by kdb mount):

- [resolver](resolver/) uses POSIX APIs to handle conflicts gracefully
- [wresolver](wresolver/) minimalistic resolver for non-POSIX systems
- [noresolver](noresolver/) does not resolve, but can act as one
- [gitresolver](gitresolver/) checks out and commits files to a local git repository
and afterwards the configuration file must be synced with
harddisc (recommended to add at every kdb mount):

- [sync](sync/) uses POSIX APIs to sync configuration file with harddisc

## Storage ##

Are responsible for reading writing the configuration to configuration
files.

Read and write everything a KeySet might contain:

- [dump](dump/) makes a dump of a KeySet in an Elektra-specific format

Read (and write) standard config files of /etc:

- [augeas](augeas/) parses and generates many different configuration
  files using the augeas library
- [hosts](hosts/) read/write hosts files
- [line](line/) reads any file line by line

Using semi-structured data for config files:

- [tcl](tcl/)-like config files (including meta data).
- [ni](ni/) parses INI files based on
    [ni](https://github.com/chazomaticus/bohr/blob/master/include/bohr/ni.h).
- [ini](ini/) parses INI files based on
    [inih](http://code.google.com/p/inih/).
- [xmltool](xmltool/) uses XML.
- [yajl](yajl/) uses JSON.

Plugins that just show some functionality, (currently) not intended for
productive use:

- [fstab](fstab/) reads fstab files.
- [regexstore](regexstore/)
- [simpleini](simpleini/) is ini without sections
- [csvstorage](csvstorage/) for csv files
- [passwd](passwd/) read/write passwd files

## System Information ##

Information compiled in Elektra:
- version is a built-in plugin directly within the
  core so that it cannot give wrong version information
- [constants](constants/) various constants, including version
  information
- [desktop](desktop/) contains information which desktop is
  currently running

Providing information found on the system not available in persistent
files:

- [uname](uname/) information from the uname syscall.


## Filter ##

*Filter plugins* process keys and their values in both
directions.
In one direction they undo what they do in the other direction.
Most filter plugins available now encode and decode values.
Storage plugins that use characters to separate key names, values or
metadata will not work without them.

- [cachefilter](cachefilter/) stores filtered keys internally so that they
  do not get accidentally lost and can be written to the storage again without
  the user having to remember including them in the writeout

### Encoding ###

Rewrite unwanted characters with different techniques:

- [ccode](ccode/) using the technique from arrays in the programming
  language C
- [hexcode](hexcode/) using hex codes

Transformations:

- [keytometa](keytometa/) transforms keys to metadata
- [rename](rename/) renames keys according to different rules

Doing other stuff:

- [crypto](crypto/) encrypts / decrypts confidential values
- [iconv](iconv/) make sure the configuration will have correct
  character encoding
- [hidden](hidden/) hides keys whose names start with a `.`.
- [null](null/) takes care of null values and other binary specialities


## Notification and Logging ##

Log/Send out all changes to configuration to:

- [dbus](dbus/)
- [journald](journald/)
- [syslog](syslog/)
- [logchange](logchange/) prints the change of every key on the console


## Debug ##

Trace everything that happens within KDB:

- [timeofday](timeofday/) print timestamps
- [tracer](tracer/)
- [counter](counter/) count and print how often plugin is used


## Checker ##

Copies meta data to keys:

- [glob](glob/) using globbing techniques
- [struct](struct/) using a defined structure (may also reject
  configuration not conforming to that structure)
- [spec](spec/) copies metadata from spec namespace
Plugins that check if values are valid based on meta data (typically
copied by another plugin just before):

- [validation](validation/) by using regex
- [network](network/) by using network APIs
- [path](path/) by checking files on filesystem
- [type](type/) using runtime type checking (CORBA types/)
- [enum](enum/) compares the keyvalue against a list of valid values
- [mathcheck](mathcheck/) by mathematical expressions using keysvalues as operands
- [conditionals](conditionals/) by using if-then-else like statements

## Interpreter ##

These plugins start an interpreter and allow you to use a bindings.

- [jni](jni/) java plugins started by jni, works with jna plugins
- [python](python/) Python 3 plugins
- [python2](python2/) Python 2 plugins (deprecated)
- [lua](lua/) Lua plugins


## Others ##

- [doc](doc/) contains the documentation of the plugin interface
- [error](error/) yields errors as described in metadata (handy for test purposes)
- [template](template/) to be copied for new plugins
- [lineendings](lineendings/) tests file for consistent line endings
- [list](list/) loads other plugins
- [filecheck](filecheck/) does sanity checks on a file
- [iterate](iterate/) iterate over all keys and run exported functions on tagged keys
- [dpkg](dpkg/) reads /var/lib/dpkg/{available,status} 
- [curlget](curlget/) fetchs configuration file from a remote host
- [shell](shell/) executes shell commandos after kdbGet, kdbSet and kdbError
- [semlock](semlock/) a semaphore based global locking logic
- [profile](profile/) links profile keys

## New Plugins ##

To add a new plugin you can copy the template plugin. Please make sure
to add your plugin:

- to this list, at least in Others
- to [cmake](/cmake/ElektraCache.cmake), at least in ALL
