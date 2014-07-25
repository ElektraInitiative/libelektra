Plugins can be mounted into the KDB and can access or manipulate the
KeySet.

# Introduction #

Elektra already has a wide range of different plugins.
The plugin folders should contain a README.md with further information.
The plugins are:

![Overview Plugins](/doc/images/overview_plugins.png)

## Storage ##

Before configuration is actually written, the file name needs to be
determined:

- [resolver](resolver) uses POSIX APIs to handle conflicts gracefully

Read and write everything a KeySet might contain:

- [dump](dump) makes a dump of a KeySet

Read (and write) standard config files of /etc:

- [augeas](augeas) parses and generates many different configuration
  files using the augeas library
- [fstab](fstab) reads fstab files.
- [hosts](hosts) read/write hosts files

Having flat, but arbitrary configuration:

- [simpleini](simpleini) is ini without sections
- [tcl](tcl)-like config files (including meta data)

Using semi-structured data for config files:

- [ni](ni) uses hierarchical INI files (with nested sections)
- [xmltool](xmltool) uses XML.
- [yajl](yajl#introduction) uses JSON.

Providing information found on the system not available in persistent
files:

- [uname](uname) information from the uname(1) tool.

## Filter ##

Rewrite unwanted characters with different techniques

- [ccode](ccode) using the technique from arrays in the programming
  language C
- [hexcode](hexcode) using hex codes

Doing other stuff:

- [iconv](iconv) make sure the configuration will have correct
  charachter encoding
- [hidden](hidden) 
- [null](null) takes care of null values and other binary specialities

## Notification ##

Send out all changes to configuration to:

- [dbus](dbus)
- [journald](journald)
- [syslog](syslog)

Trace everything that happens within KDB:

- [timeofday](timeofday)
- [tracer](tracer)

## Application ##

Applies meta data on keys:

- [glob](glob) using globbing techniques
- [struct](struct) using a defined structure (may also reject
  configuration not aligned to that structure)

## Checker ##

Plugins that check if values are valid based on meta data.

- [validation](validation) by using regex
- [network](network) by using network APIs
- [path](path) by checking files on filesystem
- [type](type) using type checking (CORBA types)

## Others ##

- [doc](doc) contains the documentation of the plugin interface
- [error](error) yields errors as described in metadata
- [success](success) is always successful
- [template](template) to be copied for new plugins
