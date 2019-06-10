# elektra-plugins(7) -- plugins overview

Multiple plugins can be mounted into the [key database](/doc/help/elektra-glossary.md) (KDB).
On every access to the key data base they are executed and thus can change
the functionality and behavior.

## Description

Elektra has a wide range of different plugins.
The plugin folders should contain a README.md with further information.
(Or follow links below.)
The plugins are:

![Overview Plugins](/doc/images/plugins.png)

### C-Interface

All plugins implement the same interface:

- `kdbOpen()` calls `elektraPluginOpen()` of every plugin
  to let them do their initialisation.
- `kdbGet()` requests `elektraPluginGet()` of every plugin in the queried
  backends to return a key set.
- `kdbSet()` usually calls `elektraPluginSet()` of every plugin
  in the queried backends to store the configuration.
- `kdbSet()` also calls `elektraPluginError()`
  for every plugin when an error happens.
  Because of `elektraPluginError()`, plugins are guaranteed to have
  their chance for necessary cleanups.
- `kdbClose()` makes sure that plugins can finally free their
  own resources in `elektraPluginClose()`.
- `kdbCheckConfig()` can be called manually to ensure a plugin is
  configured properly.
- `kdbGenConfig()` can be called to produce all valid configurations
  of a plugin.

### KDB-Interface

- To list all plugins use [kdb-list(1)](/doc/help/kdb-list.md).
- To check a plugin use [kdb-check(1)](/doc/help/kdb-check.md).
- For information on a plugin use [kdb-info(1)](/doc/help/kdb-info.md).
- For mount plugin(s) use [kdb-mount(1)](/doc/help/kdb-mount.md).

## See Also

For an easy introduction, see [this tutorial how to write a storage plugin](/doc/tutorials/plugins.md).
For more background information of the [plugins framework, continue here](/doc/dev/plugins-framework.md).
Otherwise, you can visit the [the API documentation](https://doc.libelektra.org/api/current/html/group__plugin.html).

## Plugins

### Resolver

Before configuration is actually written, the file name needs to be
determined (resolvers will be automatically added by kdb mount):

- [resolver](resolver/) uses advanced POSIX APIs to handle conflicts gracefully
- [wresolver](wresolver/) minimalistic resolver for non-POSIX systems
- [noresolver](noresolver/) does not resolve, but can act as one
- [gitresolver](gitresolver/) checks out and commits files to a local git repository
  and afterwards the configuration file must be synced with
  harddisc (recommended to add at every kdb mount):
- [curlget](curlget/) fetches configuration file from a remote host
- [blockresolver](blockresolver/) resolves tagged blocks inside config files
- [multifile](multifile/)

- [sync](sync/) uses POSIX APIs to sync configuration files with the hard disk

### Storage

Are responsible for reading writing the configuration to configuration
files.

Read and write everything a KeySet might contain:

- [dini](dini/) uses by default the ini plugin but has legacy support for dump
- [ini](ini/) supports a range of INI file formats.
- [dump](dump/) makes a dump of a KeySet in an Elektra-specific format
- [quickdump](quickdump/) uses binary portable format based on [dump](dump/), but more efficient

Read (and write) standard config files:

- [augeas](augeas/) reads/writes many different configuration
  files using the augeas library
- [hosts](hosts/) reads/writes hosts files
- [line](line/) reads/writes any file line by line
- [yajl](yajl/) reads/writes JSON.

Using semi-structured data for config files, mainly suitable for
spec-namespace (put a focus on having nice syntax for metadata):

- [ni](ni/) parses INI files based on (including metadata)
  [ni](https://github.com/chazomaticus/bohr/blob/master/include/bohr/ni.h).
- [tcl](tcl/)-like config files (including metadata).

Only suited for import/export:

- [xerces](xerces/) uses XML (without a specific schema).
- [xmltool](xmltool/) uses XML (in Elektra's XML schema).
- [simpleini](simpleini/) line-based key-value pairs with configurable
  format (without sections)
- [mini](mini/) dependency free, line based key-value storage plugin.
- [yamlsmith](yamlsmith/) exports key sets in the YAML format

Plugins that just show some functionality, (currently) not intended for
productive use:

- [typechecker](typechecker/) type checks a configuration specification
- [regexdispatcher](regexdispatcher/) generates regex representations of specification keywords
- [fstab](fstab/) for fstab files.
- [regexstore](regexstore/)
- [csvstorage](csvstorage/) for csv files
- [passwd](passwd/) for passwd files
- [dpkg](dpkg/) reads /var/lib/dpkg/{available,status}
- [mozprefs](mozprefs/) for Mozilla preference files
- [c](c/) writes Elektra C-structures (`ksNew(.. keyNew(...`)
- [file](file/) reads and writes a file from/to a single key
- [camel](camel/) reads and writes a very limited subset of [YAML][]
- [yamlcpp](yamlcpp/) reads and writes data in the [YAML][] format using [yaml-cpp](https://github.com/jbeder/yaml-cpp)
- [yanlr](yanlr/) reads data using a parser generated by [ANTLR](http://www.antlr.org)
- [yambi](yambi/) reads data using a parser generated by [Bison](https://www.gnu.org/software/bison)
- [yawn](yawn/) reads YAML data using [YAEP](https://github.com/vnmakarov/yaep)
- [yaypeg](yaypeg/) reads YAML data using a PEG parser combinators based on [PEGTL](https://github.com/taocpp/PEGTL)
- [mmapstorage](mmapstorage/) uses binary, not portable memory mapped file for a high performance storage
- [specload](specload/) calls an external application to request its specification, depends on [quickdump](quickdump/)

[yaml]: http://www.yaml.org

### System Information

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

### Filter

_Filter plugins_ process keys and their values in both
directions.
In one direction they undo what they do in the other direction.
Most filter plugins available now encode and decode values.
Storage plugins that use characters to separate key names, values or
metadata will not work without them.

- [cachefilter](cachefilter/) stores filtered keys internally so that they
  do not get accidentally lost and can be written to the storage again without
  the user having to remember including them in the writeout

**Encoding**

Rewrite unwanted characters with different techniques:

- [ccode](ccode/) using the technique from arrays in the programming
  language C
- [hexcode](hexcode/) using hex codes
- [base64](base64/) using the Base64 encoding scheme (RFC4648)

Transformations:

- [directoryvalue](directoryvalue/) converts directory values to leaf values
- [keytometa](keytometa/) transforms keys to metadata
- [rename](rename/) renames keys according to different rules
- [boolean](boolean/) canonicalizes boolean keys
- [hexnumber](hexnumber/) converts between hexadecimal and decimal

Doing other stuff:

- [crypto](crypto/) encrypts / decrypts confidential values
- [gpgme](gpgme/) encrypts / decrypts confidential values (with GPGME)
- [fcrypt](fcrypt/) encrypts / decrypts entire backend files
- [iconv](iconv/) makes sure the configuration will have correct
  character encoding
- [hidden](hidden/) hides keys whose names start with a `.`.
- [null](null/) takes care of null values and other binary specialities

### Notification and Logging

Log/Send out all changes to configuration to:

- [dbus](dbus/) sends notifications for every change via dbus `notification`
- [dbusrecv](dbusrecv/) receives notifications via dbus `notification`
- [zeromqsend](zeromqsend/) sends notifications for every change via ZeroMQ sockets `notification`
- [zeromqrecv](zeromqrecv/) receives notifications via ZeroMQ sockets `notification`
- [syslog](syslog/) logs key database changes to syslog
- [journald](journald/) logs key database changes to journald
- [logchange](logchange/) prints the change of every key on the console

Notification of key changes:

- [internalnotification](internalnotification/) get updates automatically when registered keys were changed

### Debug

Trace everything that happens within KDB:

- [timeofday](timeofday/) prints timestamps
- [tracer](tracer/) traces all calls
- [counter](counter/) count and print how often a plugin is used

### Checker

Copies metadata to keys:

- [spec](spec/) copies metadata from spec namespace (the
  standard way)
- [glob](glob/) using globbing techniques (needed by some plugins)
- [struct](struct/) using a defined structure (may also reject
  configuration not conforming to that structure)

Plugins that check if values are valid based on metadata (typically
copied by the `spec` plugin just before):

**Value Validation**

- [validation](validation/) by using regex
- [network](network/) by using network APIs
- [ipaddr](ipaddr/) checks IP addresses using regular expressions
- [path](path/) by checking files on file system
- [cpptype](cpptype/) obsolete plugin for run-time type checking (CORBA types/), use `type` instead
- [enum](enum/) compares the keyvalue against a list of valid values
- [type](type/) type checking (CORBA types) and enum functionality
- [mathcheck](mathcheck/) by mathematical expressions using key values as operands
- [conditionals](conditionals/) by using if-then-else like statements
- [required](required/) rejects non-required keys
- [date](date/) validates date and time data
- [range](range/) checks if a value is within a given range
- [reference](reference/) checks if a value is a valid reference to another key
- [rgbcolor](rgbcolor/) validates and normalizes hexcolors
- [macaddr](macaddr/) checks if MAC addresses are valid and normalizes them

**Other Validation**

- [filecheck](filecheck/) does sanity checks on a file
- [lineendings](lineendings/) tests file for consistent line endings

### Interpreter

These plugins start an interpreter and allow you to execute a script
in an interpreted language whenever Elektra’s key database gets
accessed. Note that they depend on the presence of the respective
binding during run-time.

- [jni](jni/) java plugins started by jni, works with jna plugins
- [python](python/) Python 3 plugins
- [python2](python2/) Python 2 plugins (deprecated)
- [ruby](ruby/) Ruby plugins
- [lua](lua/) Lua plugins
- [shell](shell/) executes shell commandos
- [haskell](haskell/) used for linking haskell plugins and is a small example for such plugins itself

### Others

- [doc](doc/) contains the documentation of the plugin interface
- [error](error/) yields errors as described in metadata (handy for test purposes)
- [template](template/) to be copied for new plugins
- [cpptemplate](cpptemplate/) a template for C++ based plugins
- [list](list/) loads other plugins
- [iterate](iterate/) iterate over all keys and run exported functions on tagged keys
- [process](process/) proxy plugin that executes other plugins in a separate process
- [profile](profile/) links profile keys
- [simplespeclang](simplespeclang/) simple configuration specification language
- [gopts](gopts/) global plugin to automatically call `elektraGetOpts`
- [cache](cache/) caches keysets from previous `kdbGet()` calls
