# 0.8.11 Release

From the beginning of the Elektra Initiative, Elektra aimed for avoiding
hard coded information in the application and to make the application's
configuration more transparent. While avoiding any pathes to files was
reality from the first released Elektra version, now also hard coding
default values, fallback mechanisms and even Elektra's pathes to keys
can be avoided.

How does that work?

Elektra 0.8.11 introduces a so called specification for the
application's configuration. It is located below its own namespace
`spec`.

Once the base path is known, the user can find out all Elektra
pathes used by an application, through:

    kdb ls spec/basepath

Keys in `spec` allow us to specify which keys are read by the application,
which fallback it might have and which is the default value using
meta data. The implementation of these features happened in `ksLookup`.
When cascading keys (tagged with KEY_CASCADING_NAME and starting with `/`)
are used following features are now available:

- `override/#`: use these keys *in favour* of the key itself
- `namespace/#`: instead of using all namespaces in the predefined order,
    one can specify which namespaces should be searched in which order
- `fallback/#`: when no key was found in any of the (specified) namespaces
    these keys will be searched
- `default`: this value will be used if nothing else was found

Our novel technique does not only give you the obvious advantages, but
also provides complete transparency how a program will fetch a configuration
value. In practice that means that:

    kdb get "/sw/app/#0/promise"

will give you the *exact same value* as the application uses when it
lookups the key `promise`. Many `if`s and hardcoded values are avoided,
we simply fetch and lookup the configuration by following code:

    Key *parentKey = keyNew("/sw/app/#0", KEY_CASCADING_NAME, KEY_END);
    kdbGet(kdb, ks, parentKey);
    
    ksLookupByName(ks, "/sw/app/#0/promise", 0);

We see in that example that the only Elektra pathes are hardcoded in
the application.  But those can be found out easily, completely without
looking in the source code. The technique is simple: simply append a
logger plugin and the KDB base path is printed to:

- stdout in the case of the plugin tracer
- syslog in the case of the plugin syslog
- journald in the case of the plugin journald

What we do not see in the program above are the default values and
fallbacks. They are only present in the so specification (namespace `spec`).
Luckily, the specification are key/value pairs, too. So we do not have
to learn something new, e.g. using the ni plugin:

    [promise]
    default=20
    fallback/#0=/somewhere/else
    namespace/#0=user

When this file is mounted to spec/sw/app/#0 we specify, that only the
namespace `user` should be used.

If this key was not found, but `/somewhere/else` is present, we will use
this key instead.  The `fallback` technique is very powerful: it allows
us to have (recursive) links between applications. In the example above,
the application is tricked in receiving e.g. the key `user/somewhere/else`
when `promise` was not available.

The value `20` will be used as default, even if no configuration file
is found.

Note that the fallback works on *key level*, and not like most other
systems have implemented, on configuration *file level*.


## Namespaces

The specification gives the namespaces much clearer semantics and
purpose. Key names starting with a namespace are connected to a
configuration source. E.g. keys starting with:

- `user` are keys from the home directory of the current user
- `system` are keys from the `/etc` directory of the current system

When a key name starts with an `/` it means that it is looked up by
specification. Such a key is not really present in the keyset, except
when the default value was found. Such keys are neither received
nor stored by `kdbGet` and `kdbSet`.

Applications shall only look up this way. If no specification is
present, cascading of all namespaces is used as before.

Elektra will (always) continue to work for applications that do not have a
specification. We strongly encourage you, however, to write such a
specification, because:

- it helps the administrator to know which keys exist
- it documents the keys (including lookup behaviour and default value)
- and many more advantages to come in future releases..


## Mounting specifications

Such specifications can also be used for mounting:

    kdb mount-spec "app.ini" "/sw/app/#0"

will mount the specification if app.ini has following metadata in the top-level `[]` section:

- `filename` the name to be used as configuration file
- `needs/#` the plugins to be loaded

Such specification mounts will do a cascading mount (mountpoint in every namespace)
and mount the file `app.ini` itself in the `spec` namespace.

Note that specifications are currently copied to `/usr/share/elektra/specification/`
or however CMake is configured: `@CMAKE_INSTALL_PREFIX@/@KDB_DB_SPEC@`

## API

The main API kdb.h has one added line:

    ssize_t keyAddName(Key *key, const char *addName);

This method is already used heavily in many parts. Contrary to `keySetBaseName` and
`keyAddBaseName` it allows us to extend the path with more than one Element at once,
i.e. `/` are not escaped.

A new proposed function is `keyNameGetOneLevel`. Unlike to the previous
function with the same name it will operate on null terminated strings.
Currently it has the signature:

    const char *keyNameGetOneLevel(Key *key, size_t where);

The enum for the method `keyGetNamespace` was extended according to the
new namespaces added.

Finally, a bunch of new lookup options was added, which might not be
useful for the public API (they allow us to disable the features
mentioned in the beginning).

## Obsolete and removed concepts

The concept that backends get a name is now gone. Backends will simply
be with their escaped mountpath below system/elektra/mountpoints without
any confusing additional name.

So unmounting only works with the path as given for the mount command.

Removing this concept makes Elektra easier to understand and it also
removes some bugs. E.g. having mountpoints which do not differ except
having a `_` instead of a `/` would have caused problems with the
automatic name generation of Elektra 0.8.10.

Old mountpoints, however, now need to be removed with their name
(`_` instead of `/`).

Additionally, the so called directory keys were also removed.

## parentName

While empty/invalid names are still accepted as parentName to `kdbGet`
and `kdbSet` for compatibility reasons,

    keyNew("/", KEY_CASCADING_NAME, KEY_END);

should be used instead. They have identical behaviour, except that
invalid names (that cannot be distinguished from empty names) will
produce a warning. In the next major version it will produce an error.

## Qt-Gui 0.0.3

Raffael Pancheri was very busy and did a lot of stabilizing work:

- Added markdown converter functionality for plugin documentation
- Added credits to other authors
- do not show storage/resolver plugins if a plugin of that kind has been selected
- added menu to newkey toolbar button to allow new array entries
- fixed Arrayname not shown in disabled color when creating a new array
- added option to include a configuration keyset when adding a plugin
- refactored TreeView to make it possible to define behaviour externally
- show included keys when creating the plugin configuration
- moved TreeView functions to separate file
- moved main functions to separate file
- Added all storageplugins to namefilters
- Simplified namefilters
- Reimplement ErrorDialog
- Added undo/redo of all commands and correctly update the view
- modified ToolTip size
- TreeView copy/cut branch view-update fix
- metakey view update fix when selecting a new key when editkeywindow is
- Fixed messed up keyWindow when keyname/value are very long
- changed window modality so open window is blocking input to parent/
- Color animation on search results
- Some view update fixes
- Moved AboutWindow to separate file
- Moved MainToolBar to separate file
- Moved TreeContextMenu to separate file
- Moved KeyAreaContextMenu to separate file
- Minor view fixes
- Fixed GeneralMessageDialog size issues
- GeneralMessageDialog title fix
- Need to place TreeView functions inside TreeView.qml, else they cannot
- Merge remote-tracking branch 'upstream/master'
- fixed freeze when opening UnmountBackendWindow
- fixed qml assignment errors
- Fixed undo/redo all view updates
- Refactored Buttons to accept shortcuts
- Fixed broken Next Button in Wizard
- small focus and shortcut fixes
- Updated Translations


## Bug fixing

- fix issue with escaped backslashes in front of slashes
- many fixes in docu
- atomic commits across namespaces work again
- memleak on ReadFile error in ni plugin
- `kdb getmeta` reports errorcode if key, but no meta was found
- `ksLookup` now will also work if a key of the keyset is used as
    search-key (aliasing problem fixed by dup() on namelock)
- resolver regex does not match to wrongly written plugins
- jna plugin is now named libelektra-0.8.11.jar, with proper symlink to current version, for every CMake version
- fix bashism ($RANDOM)
- new keys are correctly renamed, fixes OpenICC (thanks to Felix Berlakovich)
- comments in host keys are correctly restored (thanks to Felix Berlakovich)
- output stream in type checking is no longer locale dependent (thanks to Manuel Mausz)
- simplify CMAKE_DL_LIBS (thanks to Manuel Mausz)

## Further gems

- Examples were improved, added (e.g. cascading, namespace) and included in doxygen
- All plugins now use the cmake function `add_plugin`
   (thanks to Ian for most of the work)
- keyGetNamespace is now used internally everywhere where namespaces
    are handled. It should be much easier now to add a namespace if it
    should be required to do so.
- data directory (keysets as C-files) is now shared between different
    kinds of test suites.
- allocation tests were readded
- More compiler flags are added and many warnings are fixed
- cleanup of old unused `keyName` methods
- The key `system/elektra/mountpoints` itself was always created and a
    left-over on a freshly installed system after the unit tests run the
    first time. The physical presence of the key is now irrelevant and
    it won't be created automatically.
- Bash completion was greatly improved (thanks to Manuel Mausz)
- Configure scripts were refactored and are now much shorter (thanks to Manuel Mausz)
- New Debian build agents were added that are magnitutes faster than the old ones (a big thanks to Manuel Mausz)
- Many KDB tests, written in C, lua and python were added (thanks to Manuel Mausz)
- SWIG3 is preferred when available
- add the plugin counter that counts how often the methods of a plugin are called
- `kdb list-tools` is now advertised in `kdb --help`





# 0.8.10 Release

- guid: 6ce57ecf-420a-4a31-821e-1c5fe5532eb4
- author: Markus Raab
- pubDate: Tue, 02 Dec 2014 18:37:51 +0100

Hello,

we are delighted to announce our latest feature release providing
major updates in:

- compatibility with standards,
- tooling,
- plugins (hosts, rename),
- Qt-Gui and
- a new Java binding



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
- A new resolver variant x (for user and system) is introduced
 - implements handling of XDG environment variables
 - ignores empty dirs and absolute pathes in envvar
- add new shell based test suite for (xdg)-resolver

For example, we could use resolver_fm_xhp_x:

    kdb mount --resolver=resolver_fm_xhp_x file.dump /example dump
    kdb file user/example
    kdb file system/example

Will show you that for both user+system the resolver respects
XDG environment variables, e.g. above lines will print:

    /home/m/.config/file.dump
    /etc/xdg/file.dump

Of course, any attempts to get and set keys below user/example and
system/example will also be in these files.

The letters after _ describe the variant of the resolver:

- f .. file based locking
- m .. mutex based locking (for multiple KDB per process)
- for user configuration (after next _)
 - x .. first check XDG_CONFIG_HOME environment
 - h .. then check HOME environment
 - p .. then fall back to passwd
- for system configuration (after next _)
 - x .. check all pathes in XDG_CONFIG_DIRS and falls back to /etc/xdg

A lot of such resolver variants are added when -DPLUGINS=ALL is used.
Of course you can create new variants with different behaviour by adding
them to PLUGINS.

To make your application (that uses Elektra) XDG aware, you have nothing
to do: you get it to free. Make sure to always use cascading lookup.
Additionally, an XDG conforming application should not write system/
keys.




## OpenICC Compatibility

Based on that, Elektra now also implements the draft for
[the OpenICC specification](http://www.openicc.info/wiki/index.php?title=OpenICC_Configuration_0.1).

The mount command looks like quite complicated, but it consists of
simple parts:

    kdb mount --resolver=resolver_fm_xhp_x \
      color/settings/openicc-devices.json /org/freedesktop/openicc \
      yajl rename cut=org/freedesktop/openicc

We already know the first two lines: we use the XDG resolver already
introduced above. Only the file name and the path where it should be
mounted differs.

The plugin yajl is a storage plugin that reads/writes json. The plugin
rename was the missing link to support OpenICC (thanks to Felix
Berlakovich for closing this gap). It is needed, because every OpenICC
file starts like this:

    { "org": { "freedesktop": { "openicc": {

Because the backend is mounted at /org/freedesktop/openicc, it would
lead to keys below /org/freedesktop/openicc/org/freedesktop/openicc
which we obviously do not want. So we simply get rid of the common
prefix by cutting it out using the rename plugin.

Of course this renaming functionality can be used in every situation and
is not limited to OpenICC.




## Tools

A large number of old and new tools were added, mostly for convenience
e.g.:

    kdb mount-openicc

saves you from writing the long mount command we had in the previous
section.

To get a list of all tools that are installed, now the command (which is
also an external tool and as such currently not displayed in kdb --help):

    kdb list-tools

is available. Do not be surprised: on typical installations this will
be a large list. You can run each of these tools by using kdb <command>.
Most of the tools, however, are part of the test suite,
which you can run using:

    kdb run_all

Other tools are "old friends", e.g. convert-fstab written in 2006 by Avi
Alkalay still works:

    kdb convert-fstab | kdb import system/filesystems xmltool

It will parse your /etc/fstab and generate a XML. This XML then can be
imported. Other convert tools directly produce kdb commands, though.

kdb now uses KDB itself for many commands:

- /sw/kdb/current/resolver .. You always want a different default
    resolver than that was compiled in as default when mounting
    backends?
- /sw/kdb/current/format .. If you are annoyed by the default format
    dump format for import/export.
- /sw/kdb/current/plugins .. If you always forget to add some plugins
    when mounting something.

By default the plugin "sync" is added automatically (it makes sure that
fsync is executed on config files, the directory is already done by the
resolver), you should not remove it from /sw/kdb/current/plugins
otherwise the next mount command will not add it. To preserve it use a
space separated list, e.g.:

    kdb set user/sw/kdb/current/plugins "sync syslog"

Last, but not least, kdb get now supports cascading get:

    kdb get /sw/kdb/current/plugins

This feature allows you to see the configuration exactly as seen by the
application.

Other options:

- -123 options for hiding nth column in `kdb mount`
- hide warnings during script usage of `kdb mount`
- -0 option accepted in some tools (null termination)
- Mount got a new -c option for backend configuration. For example
  -c cut=org/freedesktop/openicc would be the parameter cut for all
  plugins. Have a look at #146 if you want to use it.



## Compatibility

The core API (kdb.h), as always, stayed API/ABI compatible. The only
changes in kdb.h is the addition of KEY_CASCADING_NAME and
KEY_META_NAME. So applications compiled against 0.8.10 and using these
constants, will not work with Elektra 0.8.9.

The constants allow us to create following kinds of keys:

- empty names: this was always possible, because invalid names
  (including empty names) did not cause keyNew to abort
- meta names: this is a new feature that allows us to compare key names
  with meta keys
- cascading names: names starting with / have the special meaning that
  they do not specify which namespace they have. When such names are
  used for
  - kdbGet() and kdbSet() keys are retrieved from all namespaces
  - ksLookup() keys are searched in all namespaces
  - ksLookupByName() is now just a wrapper for ksLookup().
      The method does not do much except creating a key and passing
      them to ksLookup().

Usage in C is:

    Key *c = keyNew("/org/freedesktop", KEY_CASCADING_NAME, KEY_END);
    Key *m = keyNew("comment/#0", KEY_META_NAME, KEY_END);

The same functionality exists, of course, in available in all bindings,
too.

Changes in non-core API are:

- xmltool now does not output default (unchanged) uid,gid and mode
- ksLookupBySpec from kdbproposal.h was removed, is now integrated into
    ksLookup
- extension keyNameGetNamespace was removed
- the hosts comment format has changed
- the default resolver has changed (uses passwd)
- kdb::tools::Backend::Backend constructor, tryPlugin and addPlugin
  have changed:
 - mountname is now automatically calculated
 - addPlugin allows us to add a KeySet to validate plugins with different
     contracts correctly
- C++ binding now throws std::bad_alloc on allocation problems (and not
  InvalidName)


## CMake

It is now possible to remove a plugin/binding/tools by prefixing a name
with "-".  The new "-element" syntax is accepted by TOOLS, BINDINGS and
PLUGINS. It is very handy in combination with ALL, e.g.:

    -DPLUGINS="ALL;-xmltool"

will include all plugins except xmltool.



## Improved comments

Comment preserving was improved a lot. Especially, the hosts plugin was
rewritten completely. Now multiple different comment styles can be
intermixed without losing information. E.g. some INI formats support
both ; and # for comments. With the new comments it is possible to
preserve that information and even better: applications can iterate
over that information (meta data).

To mount the new hosts plugin use (if you already have mounted it, you
have nothing to do):

    kdb mount /etc/hosts system/hosts hosts

The hosts plugin now seperates from ipv4 and ipv6 which makes the host
names canonical again, e.g.:

    kdb get system/hosts/ipv4/localhost
    kdb get system/hosts/ipv6/localhost

To access the inline-comment, use:

    kdb getmeta system/hosts/ipv4/localhost "comment/#0"

For other meta information, see:

    kdb lsmeta system/hosts/ipv4/localhost 

Additionally, a small API for specific meta-data operations emerges.
These operations will be moved to a separate library and will not stay
in Elektra's core library.



## Proposal

- lookup options:
 - KDB_O_SPEC uses the lookup key as specification
 - KDB_O_CREATE creates a key if it could not be found
- elektraKeyGetMetaKeySet creates a KeySet from meta data
- elektraKsFilter allows us to filter a KeySet arbitrarily (not only
    keyIsBelow in case of ksCut). It reintroduces more functional
    programming.
- keyGetNamespace was reintroduced. In one of the next versions of
  Elektra we will introduce new namespaces. keyGetNamespace allows the
  compiler to output a warning when some namespaces are not handled in
  your C/C++ code.



## Java binding

Elektra now fully supports applications written in Java and also Plugins
written in the same language.

The
[new binding was developed using jna.](https://github.com/ElektraInitiative/libelektra/tree/master/src/bindings/jna)
For the
[plugin interface JNI](https://github.com/ElektraInitiative/libelektra/tree/master/src/plugins/jni)
was used.
We developed already
[some plugins](https://github.com/ElektraInitiative/libelektra/tree/master/src/bindings/jna/elektra/plugin).


## Qt-Gui

Raffael Pancheri released the version 0.0.2 of the Qt-Gui:

* added Backend Wizard for mounting
* user can hover over TreeView items and quickly see keyname, keyvalue 
  and metakeys
* it is now easily possible to create and edit arrays
* added header to MetaArea for better clarity
* many small layout and view update fixes


## Further stuff and small fixes

- Two new error/warnings information: mountpoint and configfile.
    It is added automatically and all tools will print it.
- C++ I/O for key(s) now allows null terminator next to new-line
    terminator
- fix error plugin: now use on_open/trigger_warnings to be consistent
- fix metaset: now correctly append new key
- arrays are also available when compiled with mingw
   (but tests still have to be excluded for successful compilation)
- fix #136
- fix long help text in `kdb check`
- signed release tags are now used


## Get It! ##

You can download the release from
[here](http://www.markus-raab.org/ftp/elektra/releases/elektra-0.8.10.tar.gz)

- size: 1915277
- md5sum: 2b16a4b555bc187562a0b38919d822a1
- sha1: 08b1d0139fc5eb0d03c52408478e68b91b1825dc
- sha256: 526e2ed61e87d89966eb36ddad78d8139b976e01ce18aab340d8a1df47132355

already built API-Docu can be found
[here](http://doc.libelektra.org/api/0.8.10/html/)


## Stay tuned! ##

Subscribe to the
[new RSS feed](http://doc.libelektra.org/news/feed.rss)
to always get the release notifications.

[Permalink to this NEWS entry](http://doc.libelektra.org/news/6ce57ecf-420a-4a31-821e-1c5fe5532eb4.html)

For more information, see http://www.libelektra.org

Best regards,
Markus











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
