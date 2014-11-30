Prep for 0.8.10 Release

- guid: e3cb90f5-86f7-4e9d-8b76-2af334c48c94
- author: Markus Raab
- pubDate: Sun, 30 Nov 2014 15:15:04 +0100

## XDG Compatibility

Elektra now is
[fully XDG 0.8](http://standards.freedesktop.org/basedir-spec/basedir-spec-0.8.html)
compliant. Following changes were necessary:

- newly created configuration files for user/ now have the permissions
  0600
- newly created configuration directories for user/ now have the
  permissions 0700
- existing configuration files will retain their permissions.
- The default path to store user configuration is now ~/.config
- A new resolver variant x is introduced
 - implements handling of XDG environment variables
 - ignores empty dirs and absolute pathes in envvar
- add new shell based test suite for resolver

For example, use resolver_fm_xhp_x if you want:

- f .. file based locking
- m .. mutex based locking
- for user configuration
 - x .. first check XDG_CONFIG_HOME environment
 - h .. then check HOME environment
 - p .. then fall back to passwd
- for system configuration
 - x .. check all XDG_CONFIG_DIRS and falls back to /etc/xdg

## OpenICC Compatibility

Elektra now also implements the draft for
[the OpenICC specification](http://www.openicc.info/wiki/index.php?title=OpenICC_Configuration_0.1).


## CMake

It is now possible to remove a plugin/binding/tools by prefixing a name
with "-".
- allow to use -element syntax in TOOLS, BINDINGS and PLUGINS to remove
  it. Very handy in combination with ALL, e.g. -DPLUGINS="ALL;-xmltool"



## Improved comments

Comment preserving was improved a lot. Especially, the hosts plugin was
rewritten completely. Now multiple different comment styles can be
intermixed without losing information. E.g. some INI formats support
both ; and # for comments. With the new comments it is possible to
preserve which comment started with which -- and even better it can be
programmatically checked using the meta data.

The hosts plugin now seperates from ipv4 and ipv6 which makes the host
names canonical.

Additionally, a small API emerges for specific meta-data operations.
These operations will be moved to a separate library and will not stay
in Elektra's core library.

## Proposal

Filter?


## Tools

-c option for backend configuration



## Reintroduce namespaces

In one of the next versions of Elektra we will introduce new namespaces.


## Key Names

Different kinds of keys can now created:

- empty names: this was always possible, because invalid names did not
  cause keyNew to abort
- meta names: this is a new feature that allows us to compare key names
  with meta keys
- cascading names: names starting with / have the special meaning that
  they do not specify which namespace they have. When such names are
  used for
  - kdbGet() and kdbSet() keys are retrieved from all namespaces
  - ksLookup() keys are searched in all namespaces



## Java binding

Elektra now fully supports applications written in Java and also Plugins
written in the same language.

The [new
binding was developed using jna.](https://github.com/ElektraInitiative/libelektra/tree/master/src/bindings/jna)

For the [plugin interface
JNI](https://github.com/ElektraInitiative/libelektra/tree/master/src/plugins/jni)
was used.

We developed already [some
plugins](https://github.com/ElektraInitiative/libelektra/tree/master/src/bindings/jna/elektra/plugin).


## Qt-Gui

* added Backend Wizard
* user can hover over TreeView items and quickly see keyname, keyvalue 
  and metakeys
* it is now possible to create and edit arrays
* added header to MetaArea for better clarity
* many small layout and view update fixes




## Small fixes

- C++ binding now throws bad_alloc on allocation problems (and not
  InvalidName)


## Further stuff

Preparation for 0.8.10 release:
- fix #136
- fix long help text in `kdb check`
- kdb now uses KDB itself:
  /sw/kdb/current/resolver .. want a different default resolver than was compiled in?
  /sw/kdb/current/format .. annoyed by dump-default format
  /sw/kdb/current/plugins .. if you always forget to add some plugins
- C++ I/O for key(s) now allows null-terminator
- -0 option accepted
- -123 options for hiding nth column in `kdb mount`
- hide warnings during script usage of `kdb mount`
- add two new error/warnings information: mountpoint and configfile
- use signed release tags
- fix error plugin: now use on_open/trigger_warnings to be consistent
- fix metaset: now correctly append new key

Entries for latest Elektra for wheezy






# 0.8.9 Release

- guid: 38640673-3e07-4cff-9647-f6bdd89b1b08
- author: Markus Raab
- pubDate: Tue, 04 Nov 2014 10:48:14 +0100

Again we managed to do an amazing feature release in just two month.
In 416 commits we modified 393 files with 23462 insertions(+) and
9046 deletions(-).

## Most awaited

The most awaited feature in this release is certainly the *qt-gui*
developed by Raffael Pancheri. It includes a rich feature set including
searching, unmounting, importing and exporting. A lot of functionality
is quite stable now, even though its version is 0.0.1 alpha. If you find
any bugs or want to give general feedback, feel free to use the issue
tracker of the Elektra project. A screenshot can be found
[here](https://github.com/ElektraInitiative/libelektra/blob/master/doc/images/screenshot-qt-gui.png)
To compile it (together with Elektra), see the README
[here](https://github.com/ElektraInitiative/libelektra/tree/master/src/tools/qt-gui)

Manuel Mausz also has been very active and developed glib+gi bindings.
These bindings make Elektra more friendly to the glib/gtk/gnome world.
Using the gobject introspection python3 and lua bindings were developed.
Additionally he got rid of all clang warnings.

Felix Berlakovich also made progress: [the ini
plugin](https://github.com/ElektraInitiative/libelektra/tree/master/src/plugins/ini)
now supports multiline and
which can be dynamically turned on and off, i.e. during mounting
(thanks to Felix)

Last, but not least, Kai-Uwe ported Elektra to Windows7. MinGW is now
one more supported compiler (tested on build-server, see later).
Astonishingly, it was only little effort necessary:
Basically we only needed a new implementation of the resolver, which
is now called *wresolver*. Different from the *resolver* it lacks the
sophisticated multi-process and multi-thread atomicity properties. On
the plus side we now have a resolver that is very easy to study and
understand and still works as file resolver (_noresolver_ does not).


## Interfaces

ABI/API of the C-API is still completely stable even though under the
hood a lot was changed. All testcases compiled against the previous
version still run against Elektra 0.8.9.

This is, however, not the case for libtools. For MinGW porting it was
necessary to rename an enum related to merging because it conflicted
with an already defined MACRO.

For maintainers also some changes are necessary. For MinGW and to
actually use the flexibility of the new resolver variants two new CMake
Variables are introduced: KDB_DEFAULT_RESOLVER and KDB_DEFAULT_STORAGE.

More importantly for maintainers the CMake variables regarding SWIG
bindings are now abandoned in favour to the new variable BINDINGS that
works like PLUGINS and TOOLS. Just start with

	-DBINDINGS=ALL

and CMake should remove the bindings that have missing dependencies
on your system. Remember that glib and gi (i.e. *gi_python3* and
*gi_lua*) bindings were introduced, too. Additionally, the *cpp*
binding can now be deactivated if not added to BINDINGS.

Finally, the *gen* tool added a Python package called `support`.



## Other Bits

A proof of concept storage plugin `regexstore` was added. It allows to
capture individual configuration options within an otherwise not
understood configuration file (e.g. for vimrc or emacs where
the configuration file may contain programming constructs).

Most tests now also work with the BUILD_SHARED variant (from our
knowledge all would work now, but some are still excluded if
BUILD_FULL and BUILD_STATIC is disabled. Please report issues
if you want to use uncommon CMake combinations).

A small but very important step towards specifying configuration files
is the new proposed API method ksLookupBySpec (and ksLookup implementing
cascading search). It introduces a `logical view` of
configuration that in difference to the `physical view` of
configuration does not have namespaces, but everything is below the root
"/". Additionally, contextual values now allow to be compile-time
configured using C++-Policies. These are small puzzle pieces that will
fit into a greater picture at a later time.

A (data) race detection tool was implemented. Using it a configurable
number of processes and threads it tries to kdbSet() a different
configuration at (nearly) the same time.

With this tool the resolver could be greatly be improved (again). It now
uses stat with nanosecond precision that will be updated for every
successful kdbSet(). Even if the configuration file was modified
manually (not using Elektra) the next kdbSet() then is much more likely
to fail.  Additionally a recursive mutex now protects the file locking
mechanism.

The build server now additionally has following build jobs:

- [i386 build:](http://build.libelektra.org:8080/job/elektra-gcc-i386/):
  We had an i386 regression, because none of the developers
  seems to use i386 anymore.
- [Configure Debian Script](http://build.libelektra.org:8080/job/elektra-gcc-configure-debian/)
  Calls the scripts/configure-debian(-wheezy).
- [Local Installation:](http://build.libelektra.org:8080/job/elektra-local-installation/)
  We had an regression that local installation was not possible because
  of a bash completion file installed to /etc. This build tests if it is
  possible to install Elektra in your home directory (and calls kdb
  run_all afterwards)
- [Test bindings:](http://build.libelektra.org:8080/job/elektra-test-bindings/)
  Compiles and tests ALL bindings.
- [Mingw:](http://build.libelektra.org:8080/job/elektra-gcc-configure-mingw/)
  Compiles Elektra using mingw.

Many more examples were written and are used within doxygen. Most
snippets now can also be found in compilable files:


- [keyNew examples](https://github.com/ElektraInitiative/libelektra/tree/master/examples/keyNew.c)
- [keyCopy examples](https://github.com/ElektraInitiative/libelektra/tree/master/examples/keyCopy.c)
- [C++ deep dup](https://github.com/ElektraInitiative/libelektra/tree/master/src/bindings/cpp/examples/cpp_example_dup.cpp)
- [How to put Key in different data structures](https://github.com/ElektraInitiative/libelektra/tree/master/src/bindings/cpp/examples/cpp_example_ordering.cpp)
- [Mount some config files using augeas](https://github.com/ElektraInitiative/libelektra/tree/master/scripts/mount-augeas)
- [Mount system information](https://github.com/ElektraInitiative/libelektra/tree/master/scripts/mount-info)

Most plugins now internally use the same CMake function `add_plugin`
which makes plugin handling more consistent.

Felix converted the METADATA spec to ini files and added a proposal
how comments can be improved.

### Refactoring:

- reuse of utilities in gen code generator
- the gen support library is now in its own package (`support`)
- refactor array handling
- internal comparision functions (keyCompareByName)

### Optimization:

- lookupByName does not need to allocate two keys
- lookups in generated code
- prefer to use allocation on stack

### Fixes:

- disable cast that segfaults on i386 (only testing code was affected)
- fix keyAddBaseName in xmltool and testing code
- support non-system installation (e.g. in home directory)
- rewrote test cases to use succeed_if_same to avoid crashes on
  null pointers
- allow to use python 2.6 for kdb gen
- improve exception messages
- use memcasecmp (fix lookup ignoring case)
- fix memory leaks (ini)
- text messages for some warnings/errors
- fix many issues regarding CMake, more variants of setting CMake
  options are now allowed.
- cmake policies fixes allow us to use cmake version > 3

## Get It! ##

You can download the release from
[here](http://www.markus-raab.org/ftp/elektra/releases/elektra-0.8.9.tar.gz)

- size: 1936524
- md5sum: 001c4ec67229046509a0cb9eda223dc6
- sha1: 79ea9b83c08ed4c347ed0100b5e0e2d3309b9d04
- sha256: e0895bba28a27fb37f36f59ef77c95235f3a9c54fb71aa6f648566774d276568


already built API-Docu can be found
[here](http://doc.libelektra.org/api/0.8.9/html/)

For more information, see http://www.libelektra.org

Best regards,
Markus


# 0.8.8 Release

- guid: eca69e19-5ddb-438c-ac06-57c20b1a9160
- author: Markus Raab
- pubDate: Tue, 02 Sep 2014 17:31:42 +0200


In this release we changed 578 files in 473 commits
(68596 insertions(+), 59260 deletions(-) compared to Elektra 0.8.7).
We assume thats the largest change set for any of Elektra's releases
up to now. It happened only within a bit more than a month up
(0.8.7 was released 28.07.2014).


## New features

GSoC finished successfully (thanks Ian and Felix)
See http://community.libelektra.org/wp for the latest results.
So Elektra now has a 3-way merging framework that is superior
to text-based merging in many scenarios (e.g. moving configuration
options within a file or with in-line comments) iff a storage plugin
creates key names that are not only line numbers.
We love to get Feedback!

Writing plugins is now even more comfortable.
A plugin writer tutorial was written (thanks Ian):
https://github.com/ElektraInitiative/libelektra/blob/master/doc/tutorials/plugins.md
The documentation was completely reworked:
http://doc.libelektra.org/api/0.8.7/html/group__plugin.html
And two new macros allow printf formating for warnings and errors
(ELEKTRA_ADD_WARNINGF and ELEKTRA_SET_ERRORF).

The ini plugin was greatly improved (tested with samba configurations
and added to ALL plugins) and the hosts plugin was rewritten to support
ipv6 properly (thanks to Felix).

The constants plugin was added and allows introspection of Elektra's
cmake variables. Because such non-file based plugins (e.g. also uname)
do not need resolving, the plugin noresolver was added. It supersedes
the success plugin.

Elektra now allows to correctly fsync its configuration files
(sync plugin) and the folders where files are stored (resolver plugin).
Just make sure to add the "sync" plugin using kdb mount.
The resolver plugin now reads from passwd and no longer needs
environment variables.  Additionally, the resolver plugin was prepared
to support other variants by so called compilation variants.

The error plugin now allows, next to list all possible errors, to
provoke errors when opening plugins. We fixed some issues related
to plugins having errors when they initialize themselves.

So following plugins were added: sync noresolver line ini constants
Nearly all plugins now have a README.md for further information
(thanks to Ian). An overview of all plugin is on with links to them:
https://github.com/ElektraInitiative/libelektra/blob/master/src/plugins/

The kdb tools were greatly improved (thanks to Felix):
- added remount tool
- umount now also accepts mountpath
- mount allows to specify different resolvers
- import now can use merge strategies
- check without arguments checks key database
- mount is now more verbose when validation fails

New/improved scripts/make targets (note that scripts can be executed by
kdb scriptname):
- mounting, unmounting scripts were added
- generate template for a new plugin was improved
- configure-debian was added
- added targets run_all and run_memcheck
- bash completion file now installed
- ucf integration
- merging scripts were added for the usage with ucf
- scripts doing internal checks on source of plugins


## Compatibility

This time we had to break compatibility. We did not change the ABI (your
application still will be able to use Elektra 0.8.8) and we did not
change the API (your application still will compile against Elektra). We
changed the third part of our interface: the semantic interface.

The problems were following:
keyAddBaseName/keySetBaseName did something obvious when no special
characters were in the baseName. But once there were, there are two
different interpretations what it should do:
1.) add/set a basename, so escape characters that are not canonical
    in the basename
2.) add all parts of the name given (with slashes)

The methods were used in both ways, so it was obvious that something is
very wrong. We decided that it should do what the name says, that is
add/set a basename (variant 1).

The variant 2, to add any name was added and is called keyAddName() and
added as proposal.

(Thank Felix for implementations and Manuel for investigations)

When keys are renamed after adding to a keyset is a bad thing because it
destroys the order of the keyset. This is now avoided by keyLock.
Use keyDup() to get rid of such locks.

Another, even larger, change is also about ordering of keys in keysets.
Elektra now internally has an null-terminated unescaped keyname.
Ordering of keysets will always happen on this name. The keyCmp() tool
can be used to check this order. It works very efficiently with
memcmp() and never gets confused by ASCII ordering of / (because / is
0 in the unescaped keyname).

The syntax, semantics and conventions of key names is now documented in
detail:
http://doc.libelektra.org/api/0.8.8/html/group__keyname.html

ksNew() does now return a keyset with a properly set cursor (ksRewind).

Because its always possible that software relies on bugs the
better way to deal with such a situation (as the keySetBaseName()
situation described above) is to provide the same function twice.
Manuel said he will create a prototype to introduce symbol versioning
in Elektra. With that, old customers would still receive the old
behaviour, but people compiling against a new version would get the new
behaviour. So in one of the next releases we will also avoid semantic
interface changes when there is a valid use case for it (there is none
if the program e.g.  crashes).

Symbol versioning also allows to compile against old versions on
purpose if you do not want the new behaviour.

We have prepared an ABI-test suite, that also checks behaviour,
for that purpose, but we also improved testing in other parts:
- (New Test strategy)[/doc/TESTING.md]
- New resolver tests for conflicts (needs tty)

If you try to execute test_ks from 0.8.7 with libelektra 0.8.8 it will
crash, but not because of any incompatibility, but because of strcmp in
the test itself gets a null pointer. The pointer is now null, because
ksNew correctly rewinds its internal cursor (see above). Amusingly,
it says on that line 94 in test_ks.c:
  // TODO: why is the cursor here?

## API Proposals

see above for more information:
- keyAddName         ..  add key name without escaping, like keySetName
- keyUnescapedName   ..  get access to null-separated unescaped name
- keyLock            ..  to allow to secure keys against modifications

some new ideas:
- keySetStringF      ..  printf format-style changing of the key string
- elektraKeySetName  ..  to allow to set meta + cascading keys

elektraArrayIncName() now works correctly with empty arrays embedded in
other arrays (yajl+line plugin)

elektraArrayValidateName() was also added, thanks to Felix.

These methods are declared in the file kdbproposal.h
but do not guarantee any forms of compatibility (they might
even be removed).


## Issues

Many issues were resolved as you can see in github:
https://github.com/ElektraInitiative/libelektra/issues
Alone for the milestone 0.8.8 we closed 17 issues, including
those mentioned in "Compatibility". Other issues (not all were
tracked on github):

- fix undefined errors in kdbOpen() or kdbClose()
- Now Python 2+3 work in parallel
- python2 interpreter is found correctly (also on Arch)
- Sentinel now makes sure that you cannot forget KS_END to end ksNew
- Fixes for architecture-specific problems by Pino
- fix .pc file
- fix compilation problem with KDB_MAX_PATH_LENGTH
- tmpnam to mkstemp (security)
- make test data naming consistent (thanks Pino)
- use LIB_SUFFIX for TARGET_TOOL_EXEC_FOLDER thanks to Kai Uwe
- Fix search for boost (thank Pino)

## Other Stuff ##

Thanks to Pino Toscano Elektra 0.8.7-4 is now available in Debian
Testing: https://packages.debian.org/search?keywords=elektra
So it is only a matter of time that other (debian-based) distributions
will follow and replace the dusty Elektra 0.7.

Debian Continuous Integration http://ci.debian.net/packages/e/elektra
(thanks Pino)
greatly complement our tests running on http://build.libelektra.org:8080/

Elektra's buildserver also was improved:

 - now also compiles with icc
 - runs make run_memcheck
 - checks if plugins are added correctly in-source
 - runs ABI + behavioural tests

Raffael Pancheri now made a merge request for qt-gui
https://github.com/ElektraInitiative/libelektra/pull/103/files
in which copy, paste and delete of keys already works.
It is, however, still work in progress.

Manuel Mausz made great progress in script-based Elektra plugins. He is
also working on glib+gobject-introspection based bindings.
He investigated some issues, e.g. a crash of the python binding which
was only triggered if python3 is build with a specific flag/module
combination, see:
https://github.com/ElektraInitiative/libelektra/issues/25


## Get It! ##

You can download the release from:

http://www.markus-raab.org/ftp/elektra/releases/elektra-0.8.8.tar.gz

- size: 1644441
- md5sum: fe11c6704b0032bdde2d0c8fa5e1c7e3
- sha1: 16e43c63cd6d62b9fce82cb0a33288c390e39d12
- sha256: ae75873966f4b5b5300ef5e5de5816542af50f35809f602847136a8cb21104e2


already built API-Docu can be found here:

http://doc.libelektra.org/api/0.8.8/html/

Best regards,
Markus
