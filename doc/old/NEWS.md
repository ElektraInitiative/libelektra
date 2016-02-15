# Augeas and Config::Model

- author: Markus Raab
- guid: eca69e19-5ddb-438c-ac06-57c20b1a9160
- pubDate: Mon, 22 Oct 2014 17:31:42 +0200

A common question is: now we have Augeas for editing config files,
why do we need Elektra, Config::Model or something else?

First, it is clear that Augeas is a huge step forward and improved
configuration of Linux systems, especially when used with centralized
configuration management tools.

Augeas, in short, introduces a special-purpose programming language that
allows to transform configuration files into configuration trees and
back. This transformation is it strength (so it is easy to add support
for a particular legacy configuration files), but also it weakness (the
mapping is implementation-defined by a language that is limited to a bit
more than regular expressions). Augeas is not able nor is it intended
to provide more abstraction over the configuration files. Instead Augeas
mirrors the structure of the configuration as closely as possible.

Elektra's goal, instead, is not only to provide access to legacy
configuration files, but to provide access to the configuration
exactly as the programs itself uses it. So with Elektra, the developers of
applications are part of Elektra's ecosystem by providing specifications
how their configuration should look like and by writing plugins that
define how the configuration is accessed and checked. Ideally, after
some time of legacy issues and migration, developers will also not
have to care about writing plugins anymore, but just use any available
ones (and users of their application can choose any other compatible
plugin).  What is about to stay is a specification that defines the
application's configuration, e.g. in INI (could be any syntax):

    [/yourapp/file_dialog/show_hidden_files]
    type=Boolean
    default=true

allows other applications to reuse your setting show_hidden_files by
referring to above specification. So Elektra not only abstracts from
cross-platform-related issues with an consistent API, but also allows
us to be aware of other applications' configurations, leveraging easy
application integration.

Config::Model shares most of Elektra's goals, especially those regarding
validation (you saw the type=Boolean above) and having a unified
interface for all programs (this feature is unavoidable with any such
approach). The projects mainly differs that Elektra is supposed to be used
by the programs themselves (and not only by GUIs and validation tools)
and that Elektra uses self-describing data: the specification itself
is also in Elektra's key database, stored in meta data and e.g. below
system/elektra/mountpoints. In Elektra validators can be
written in any language (because the specification is just data)
and can enforce constraints on any access (because plugins define
the behaviour of the key database).






# 0.8.7 release

- author: Markus Raab
- date: 28.07.2014

Again, we managed to have a great feature release with dozens of
corrections!

## New Features ##

Thanks to Manuel Mausz for further improving lua, python3 bindings
and the new python2 bindings.

The GSoC efforts have their first large contribution to Elektra, the 3
way merge finally arrives with this release. It is still a long way to
go, however, because augeas plugins can only be mounted with a
workaround and the package integration of the 3-way merge is still in
its infancy. More information about GSoC and its progress can be found
[here](http://community.libelektra.org/wp).

A special thanks to  Felix Berlakovich for his contributions to the
3-way merge, including meta merging, conflict resolving strategies and
extensive testing.

Additionally, he added the plugins keytometa, ini and greatly improved
the glob plugin. These plugins are technical previews and will receive
some improvements in the next release, too.

Now a script for tab completion is available
[here](/scripts/scripts/kdb-bash-completion), again thanks to Felix
Berlakovich.

The contextual values now got a [tutorial]
(https://github.com/ElektraInitiative/libelektra/tree/master/src/tools/gen)
and small fixes.


## Corrections ##

Thanks to Pino Toscano for fixing a lot of spelling errors, simplify
RPATH setting, respect $HOME and $TMPDIR, improvements of test cases,
and his debian-packaging efforts.

In the kdb tool not allowed subfolders are now checked properly
and the output of warnings comes before output of the error.
This fixes the problem that in the case of a longer list of
warnings one did not see the error anymore.

Fix compilation warnings on clang and gcc 4.9.
Also improve test coverage on kdb tool and some plugins.

Fix kdb import/export for some plugins (Should now work with any storage
plugin again).

kdb run_all should run flawlessly with this release. Remaining problems
with not installed test data were fixed. kdb run_all also checks if the
test cases do not modify any existing key and keeps a backup if this
happened.

Some remaining mem leaks in rare circumstances were fixed. Valgrind
should now never report any leaks, if it does, please report the issue.


## API Changes ##

Added delMeta() for C++, because setMeta() with NULL will set the
number 0 and not remove the meta.

Arguments of isBelow, isDirectBelow, isBelowOrSame are swapped for
better readability. k.isBelow(root) now means the obvious thing.
The change only effects the C++ binding, keyIsBelow is unaffected by
the change.


## Documentation ##

[Specification of meta data](doc/specification.ini) and
[contracts](doc/contracts.ini) written/greatly improved.

[Decisions](decisions) are introduced again.

Most often the KeySet is ideal, e.g. when doing full iteration or when
performing set operations. In some cases, however, a hierarchical data
structure fits better. This is especially true for GUIs. Luckily, Keys
can be in multiple data structures because of their reference counting.


## Other Stuff ##

We now fully embrace github:
- We use its issue tracker (all issues from local text files were moved
  there)
- We have rewritten many READMEs to use githubs markdown
- On pull requests the build server checks if the merge would break
  the build.
- All previous gitorious users are now at github. (Most had an account
  anyway)

Raffael Pancheri also made progress with its qt-gui. It now features a
model that implements great parts of Elektra's features. Unfortunately
the model cannot be serialised and thus changes cannot made persistent.
Also undo and other important use-cases are still not there. The GUI
looks very clean and was evaluated in a SUS study on 23.07.2014.
The current implementation can be found [here]
(https://github.com/0003088/qt-gui).

Many distributions already have Elektra packages
 - [Fedora](https://admin.fedoraproject.org/pkgdb/package/elektra/)
 - [Gentoo](http://packages.gentoo.org/package/app-admin/elektra)
 - [Arch Linux](https://aur.archlinux.org/packages/elektra/)

In some distributions Elektra packages are available, but are not
up-to-date. Pino Toscano is working on get them (actually
Debian, but others are derived from it) up-to-date.
 - [Ubuntu](https://launchpad.net/ubuntu/+source/elektra)
 - [Debian](https://packages.debian.org/de/wheezy/libelektra3)
 - [Linux Mint](http://community.linuxmint.com/software/view/elektra)

A special thanks to Kai-Uwe Behrmann for providing packages for
[CentOS, Fedora, OpenSUSE, RHEL and
SLE](http://software.opensuse.org/download.html?project=home%3Abekun&package=elektra).


## Get It! ##

You can download the release from:

http://www.markus-raab.org/ftp/elektra/releases/elektra-0.8.7.tar.gz
size: 1566800
md5sum: 4996df62942791373b192c793d912b4c
sha1: 00887cc8edb3dea1bc110f69ea64f6b700c29402
sha256: 698ebd41d540eb0c6427c17c13a6a0f03eef94655fbd40655c9b42d612ea1c9b


already build API-Docu can be found here:

http://doc.libelektra.org/api/0.8.7/html/

Best regards,
Markus



-------------

Date: 21.06.2014
Subject: Move to Github, Code Coverage Report

The Elektra Initiative moved to Github for following reasons:

- We were seriously missing a bug tracker
- It may make collaboration easier
- Build Server Integration:
 - [Build on MergeRequests](http://build.libelektra.org:8080/job/elektra-mergerequests/)
   waits for the first merge request to be built!
 - And the build status is now published [for this job](http://build.libelektra.org:8080/job/elektra-gcc47-all/)


The main entrance point and URL for any advertisement and linking stays

  http://www.libelektra.org

(which points to github at the moment)


Additionally (not related to github), we now have a
[LCOV code coverage report](http://doc.libelektra.org/coverage/latest/)!


best regards,
Markus




Subject: 0.8.6 Release

Hello List,

this release adds both many improvements and some technical previews.



== Improvements ==

The hosts plugin got documentation and several bug fixes.
Multiline comments now remove the comment start sequences within
the metadata comment.
Additionally the kdb tool has an improved error message on invalid
filenames. (thanks to Manuel Mausz)

Fix a issue that the resolver plugin did not delete tmpfile in some
error situations and add testcases with error plugin.

Fix KS_END in C++ that did not specify the namespace.

"kdb run_all" will now run all tests using Elektra from your system.
Because not all testdata will be installed, some testcases fail unless
you copy the data manually. Those are:
testmod_yajl testmod_augeas testmod_fstab testmod_hosts test_xml

Virtually all clang compiler warnings were fixed
(thanks to Felix Berlakovich).

kdb mv now is atomic. (API was used wrongly in the tool)

Clang, icc and gcc are now all supported and tested.


== Technical Previews==

A plugin that logs write operations and errors via the native
journald interface was added (thanks to Felix Berlakovich).

A preview of the augeas plugin was added. In cmake add "augeas" to PLUGINS
and then run "kdb info augeas" for further instructions. Additionally,
see Outlook below (thanks to Felix Berlakovich).

A preview of python3 and lua bindings were added. In cmake enable the
options BUILD_SWIG BUILD_SWIG_LUA BUILD_SWIG_PYTHON so that they are
compiled (thanks to Manuel Mausz).

The code generator was extended to support contextual values, more
information about that topic later.

The libtools library is back to life again! It has the same idea as its
predecessor, but it is much more powerful. Because of the plugin system
libtools is able to export and import any configuration format
Elektra supports. Additionally it adds support for mounting backends.
While most of its functionality is mature, the API is not final, though.



== API Changes ==

The API and ABI is as always backwards-compatible within the 0.8 series.

This time it is, however, not forward-compatible (that means programs
linked against 0.8.6 might not work with 0.8.5), because ksAtCursor()
was added.

ksAtCursor() provides the means for an external iterator. This
was immediately exploited by an implementation of the C++
iterators. These new C++(11) (reverse)iterators allow multi-pass
algorithm over the same KeySet.

ksSort was in the kdb.h header file, even though it never was
needed nor available in 0.8. Automatic compatibility checkers may
wrongly tell that the API is not compatible, but this is not the case
(thanks to Manuel Mausz).

The C++ API changed, including:
- does not wrongly convert garbage to default types using ```get<T>```
- getMeta now interally uses ```get<T>``` and both throw the
  KeyTypeConversion Exception
- KeySet::at directly allows one to use ksAtCursor()
- KeyTypeConversion (or former KeyBadMeta) is not thrown anymore if key
  is not available
- Key::hasMeta allows one to directly check if meta data is available

CMake: ENABLE_CXX11 is now default OFF (because gcc 4.6 and older won't
       work with it). It disables some tests, though.
unique_ptr will automatically be used instead of auto_ptr in this mode.

Note for maintainers: libelektratools now needs to be installed so that
the kdb tool will work.


== Outlook ==

One drawback of Elektra is the small number of available storage
plugins. While writing storage plugins isn't too hard, it still requires
knowledge of Elektras plugin architecture. In addition it is always
necessary to keep both translation directions in mind (from file to
Elektra and back).  In order to mitigate this issue Felix Berlakovich
is working on an Elektra storage plugin which uses Augeas [1] to read
and manipulate configuration files. Augeas uses a concept called lenses
to translate between files and abstract trees (i.e. trees that hide
unimportant details like whitespaces). In brief, what makes lenses special
is the fact that they automatically disallow translations that cannot be
reversed. This means that one of the translation directions comes for free
as it is implicitly described by the other one. Therefore writing lenses
is much simpler than programming both directions explicitly as done in
traditional Elektra storage plugins.  Furthermore many lenses for common
applications like Apache, MySQL, DHCPd and many more [2] already exist.
Besides these benefits, Augeas lacks many features of Elektra. Although
this is mostly intended because Augeas aims to concentrate just on
configuration file manipulation some of these features would be valuable
for administrators and developers. Some of these mentioned Elektra
features include but are not limited to: logging of all configuration
changes done, transformation of character encoding as needed, import and
export of configuration data in any format, e.g. XML or JSON (Augeas has
a similar feature for XML), file conflict detection and several different
validations for configuration options (see [3] for a full list of
features). With the Augeas storage plugin these features cannot be used
with only the applications already integrated into Elektra, but with all
applications for which lenses exist.  This storage plugin targets system
administrators as well as developers, that wish to take advantage of
Elektras features, but were unable to do so because of missing plugins. If
no lens exists already, writing a new one requires neither programming
skills nor knowledge of Elektras plugin architecture. Furthermore,
implementing a new configuration file format is just a matter of writing a
lens instead of writing a full-fledged parser.
[1] http://augeas.net/
[2] http://augeas.net/stock_lenses.html
[3] http://www.libelektra.org

Since Elektra is written in C, integration is limited to projects
written in C and languages compatible with C like C++. In order to
change this situation Manuel Mausz is working on the implementation of
language bindings using two different techniques.  The first technique
uses SWIG as a generic generator for various languages. In a nutshell,
SWIG is a compiler that takes C/C++ declarations and creates the wrappers
needed to access those declarations from other languages. The second
technique is called GObject Introspection. In contrast to SWIG, GObject
Introspection generates a language independent metadata file. For dynamic
languages the GObject Introspection support in the target language will
load this metadata file to generate bindings at runtime. In order to
provide bindings for static languages a compiler on the metadate file
will be used. This is comparable to using SWIG. Manuel will focus on
implementing language bindings for Python and Lua though both techniques
will support various other languages.  As language bindings only allow
to call into Elektras API Manuel will also embed the interpreters of
both Python and Lua into two generic plugins. Using these plugins in
combination with the languages bindings it will be fully possible to
write plugins for Elektra in languages other than C/C++.

While Elektra offers sophisticated methods to store and manipulate
configuration, currently there is only a command line tool (kdb)
available to interact with the database. This requires the user to study
and learn the possible commands and does not allow free exploration of the
options of the application.  To enable new ways to interact with Elektra,
Raffael Pancheri is designing and implementing a Graphical User Interface
(GUI). To increase the probability to create a GUI that is indeed usable
and beneficial, Pancheri is following the design principles proposed by
Ben Shneiderman and Jakob Nielson.

Finally, as you already noticed, Ian Donnelly is working on a semantic
3-way merge to make distribution and package upgrades smoother.
This is done as GSOC project. The progress is documented here:
http://community.libelektra.org/wp/


== Get It! ==

You can download the release at:
 http://www.markus-raab.org/ftp/elektra/releases/elektra-0.8.6.tar.gz
 size: 1188337
 md5sum: 4a59824e70a29295e9ef9ae7605d9299
 sha1: 2570710b0057470223611ca00d61a0196e54e7b2
 sha256: e815cf69b070c339784472841aa0ee0b169fab7c78f41cbbd7044f53fa9ed216


Docu can be found here:
 http://doc.libelektra.org/api/0.8.6/html/

You can install the debian packages for debian (wheezy, amd64 only,
some packages will be added later) by adding following lines to your
/etc/apt/sources.list:
 deb http://markus-raab.org/ftp/wheezy wheezy main

and install or upgrade the packages with:
 sudo apt-get update
 sudo apt-get install libelektra-core4 libelektra-full4 libelektra-bin libelektra-dev libelektra-test libelektra-xmltool4 libelektra-json4 libelektra-dbus4 libelektra-doc


Best regards,
Markus






-------------

0.8.5 Release

In this release, again, amazing features wait for you.

kdb tool now smoothly integrates external tools. These tools can reside
in external source repositories and even written in other programming
languages. The approach is similar from "git". Any executable in the
folder, given with the CMake Cache variable TARGET_TOOL_EXEC_FOLDER
(default "/usr/lib/elektra/tool_exec"), can be executed by

   kdb name

All tests (script+compiled) now install in this folder instead of
directly to /usr/bin to minimise cluttering folders in the user's PATH.

The CMake variable TOOLS, similar to PLUGINS, now allows you to decide
which tools should be built and installed. Default is "kdb" only.

The first new tool that makes use of this feature creates a powerful
alternative to directly use Elektra's API. The tool "gen", executed with
"kdb gen" can generate code and documentation for your specific
application with your specific types. The code includes getopt parsing
(short+long options), high level getter and setter in C and C++. The
documentation includes html, doxygen and troff (manpages). The
full implications of this holistic approach cannot be summarised in
one paragraph, but they include better type safety and especially
much faster development. Give it a try, examples are included in the
folder "src/tools/gen" (see Makefile) and external tools using it will
be released soon
(see https://gitorious.org/elektra-tools/elektra-tools/).

For the code generation clear mappings from Elektra's type system to
C/C++11 types are needed. The new header file kdbtypes.h introduces
these mappings using CORBAs mappings for types -- except wchar_t was not
included, because imho it is useless to have a wide char as long it is
not clear if it is 16 or 32 bits. But also included are including enum
and string types.

Unfortunately, the freedesktop-resolver features for OpenICC have to be
moved to the next release.

The documentation's installation directory now can be configured more
fine-grained (the old TARGET_DOCUMENTATION_FOLDER is gone):
TARGET_DOCUMENTATION_HTML_FOLDER
TARGET_DOCUMENTATION_LATEX_FOLDER
TARGET_DOCUMENTATION_MAN_FOLDER

Man pages now have the default location /usr/share/man/man3 and all
man pages are in namespace 3elektra to not conflict with other man
pages.

Test data for the test cases will be installed, too. The path can be
configured with:
TARGET_TEST_DATA_FOLDER
The path allows one to run tests from an installed version.

Additionally, many small bugs were fixed. The release is, as always,
100% binary and API compatible, so you can drop it in and all
applications continue to work.



You can download the release at:
 http://www.markus-raab.org/ftp/elektra/releases/elektra-0.8.5.tar.gz
 size: 1193948
 md5sum: 6fe4a48d70cefc04c04639e5d85a0ddc
 sha1: 7e26ce19a186d96091bfc0ec3ce4013991082a1c
 sha256: 4ff9fc35ce9354d3bfe7877ae7ac3426621952c974feaab8d1ad20f72c4fb9ce

Docu can be found here:
 http://doc.libelektra.org/api/0.8.5/html/

You can install the debian packages for debian (wheezy, amd64 only;
gen tool, and "all" package will be added later) by
adding following lines to your /etc/apt/sources.list:
 deb http://markus-raab.org/ftp/wheezy wheezy main

and install or upgrade the packages with:
 sudo apt-get update
 sudo apt-get install libelektra-core4 libelektra-full4 libelektra-bin libelektra-dev libelektra-test libelektra-xmltool4 libelektra-json4 libelektra-dbus4 libelektra-doc



---------------

0.8.4 Release

I am proud to announce this amazing feature release!

Elektra now speaks json!
From this release on, Elektra has full read and write capabilities
towards the world of json. json is a very popular serialization format
which is originated in Javascript.
It is very similar to the already existing tcl plugin, which also works
like a parser without intermediate datastructures. But unlike the tcl
plugin, where a grammar was written, the blazing fast yajl library was
used.

Elektra now has convention for arrays!
Multiplicity is a very common need within configuration. Json specifically
has syntactic support for it. Up to now Elektra used different conventions
for arrays. E.g. Mountpoints name their plugins #0-#9, and fstab had
#00-#99 lines. These approaches
always limited to  number of predefined elements.

The new convention solves the problem: #0-#9 (unmodified to be compatible
with mountpoints) is followed by #_10-#_99, continuing with #__100 -
#__999 and so on. It gets a bit long for large arrays, but they are
supposedly not edited by hand anyway.  This approach has the big advantage
that it works without modifying the sort order for the KeySet., which
cannot be done for two reasons:

1.) Backwards compatibility: It would break applications which count on
5 being ordered after 12. The compatibility problem could be tackled in
Elektra 0.9, but:

2.) Slower comparison is the main issue. Elektra heavily depends on
comparing keys. It is needed for inserting, lookups and other KeySet
operations.
Currently comparison is done by strcmp() which is highly optimized on
most platforms. When numbers need some special handling, strcmp() could
not be used anymore.

POSIX compatibility and handling of multi-process conflicts has
been improved. Now the HOME environment variable will be used. The
old behaviour (USER environment variable) is still available as
fallback. Additionally there is a fallback for embedded systems to the
hardcoded variant. In case of these fallbacks a warning will be added.

A new plugin uname was added. It allows you to mount the functionality
of uname within Elektra's key/value namespace. Currently it is read-only
and needs POSIX.

The built-in description of plugins, especially for new plugins, was
extended and improved. It is now also possible to output the description
by:
 kdb info <plugin> description
e.g.
 kdb info yajl description
 (the rebase plugin mentioned there did not made it into the release)

Two new kdb commands were added:
 kdb file
allows you to print out the configuration file where a given key is located.

 kdb sget
guarantees you to get a value which is handy in shell scripts.


Now a large test suite makes sure that the kdb tool works without
troubles. Many bugs, especially concerning resolver and mounting, were
fixed. The list is way too long to put it here, see git log for details.


The build server now has a job searching for TODOs:
 http://build.libelektra.org:8080/job/elektra-todo/

The latest documentation from git master is also build there and the
results are published here:
 http://doc.libelektra.org/api/latest/html

The current release is always documented at:
 http://doc.libelektra.org/api/current/html

To get exactly this version you can look at:
 http://doc.libelektra.org/api/0.8.4/html

You can download the release at:
 http://www.markus-raab.org/ftp/elektra/releases/elektra-0.8.4.tar.gz
 size: 1153568
 md5sum: 247498bf77e6c60b02d67d32e69fc963
 sha1: 32ef1f5f31956c43bda1494c864012c98a930f75
 sha256: 06be5ebe240f8c0cbbafb30dc061176760a5c66a1675165af2736c15923c84c2

You can install the debian packages for debian (wheezy, amd64 only) by
adding following lines to your /etc/apt/sources.list:
 deb http://markus-raab.org/ftp/wheezy wheezy main

and install or upgrade the packages with:
 sudo apt-get update
 sudo apt-get install libelektra-core4 libelektra-full4 libelektra-bin libelektra-dev libelektra-test libelektra-xmltool4




---------------

Roadmap

Exciting times for software integration for free operating systems:
After some years of more or less deep sleep I will reactivate the
initiative so that we finally can lay a foundation for a truly integrated
system. During my PhD I will have time to remove the larger barriers and
tackle some fundamental problems.

The targets will be:
- Embedded: Elektra is on the frontier for embedded systems because of
  its tiny core and the many possibilities with it's plugins.
- Server: Elektra is ideal suited for a local configuration storage by
  mounting existing configuration files into the global tree. Nodes
  using Elektra can be connected by already existing configuration
  management tools.
- Desktop: Elektra allows applications to read and write from a global
  configuration tree. Missing is a description (schema) so that these
  values actually can be shared.

Quality Goals will be:
- Extensibility: Requirements for configuration can vary a lot.
  The only common dominator are key/value pairs, nearly everything else
  can differ from project to project. So Elektra needs to be, and is,
  very extensible to fulfil this wide range of goals.
- Simplicity: The system should be very simple in its core so that it
  can be understood easily. Complexity should be hidden in optional
  extensions. The full system should work out-of-the-box as much as
  possible. With these properties it should also be as safe and
  fault-tolerant as possible.
- Performance: Features will not be used if they impact performance too
  hard. So Elektra and it's extensions are designed to perform well with
  a minimum amount of syscalls with an emphasis on retrieving configuration
  fast.


The roadmap for Elektra's implementation has three steps:

1.) Elektra 0.8
Is the current stable release branch. It will get continuous development
with releases at least every two month. They focus will be on stability,
better user experience, integration of new programming languages and
more serialise formats.
Some ideas:
- serialiser: e.g. json, using lenses
- resolver: e.g. xdg with improved locking
- testing: automated abi compatibility checks, shell test suite
- bindings: gobject, swig
- more tutorials and explanations
- more advertisement (e.g. conferences)
- creating applications using Elektra
- schema for configuration
- more tool support, e.g. GUIs
- and much more...
(see also doc/todo/CURRENT and doc/todo/TODO in repo)

If you are interested in working on one of these topics, please contact me.
It will also be possible for students to write a thesis solving problems
concerning those ideas.

2.) Elektra 0.9 (starting around 2014)
This will be a development branch which will coexist next to 0.8. In it
all features will emerge which are too large for the 0.8 branch, like
changes in the core and the plugin system. Some of them might be "boring
and academical" :-)
The branch will lead to releases which can be considered as technology
previews. They will be most likely not intended to be used in a
production environment.
Some ideas:
- allow plugins to be first level citizens
- allow plugins to nest in each other
- allow non-file based plugins
- Next generation of type system allowing user defined types, subtypes,
  units and constraints
- Next generation of contracts which allow recursive nesting and
  auto-negotiation of capabilities
- and much more...
(see also doc/todo/FUTURE in repo)

Those tasks will be done by me, but ideas and discussions are always
appreciated.

3.) Elektra 1.0
This will basically a stable snapshot of 0.9 when there is no doubt that
Elektra can be extended without breaking your (the customers) way to
deal with configuration.
Some ideas:
- API cleanup
- build an environment around the libraries
- build sharing platforms for configuration
- extend the foundations created in 0.9
- and much more...

More information will be published soon, so stay tuned.


---------------

0.8.3 Release

This release mostly brings improvements in the release process, the test framework and
new debian packages.

A benchmark showed that rereading the same database only needs
about 2ms and the syscalls needed are quite optimal.

The debian folder is now removed from master repository,
but instead a release and debian branch was created.
See doc/GIT for new policy of branches.
Thanks to Christian Amsüss <chrysn@fsfe.org>
for all tips, suggestions and help.
The git branches are done like in arandr.
0.8.2 was skipped because a git tag was placed wrongly
when the new release scheme was tried out on Monday June
18 2012.
The tag was removed later.

kdb export now works nicely, use
 kdb export <path> simpleini
to show key = value pairs.
Additionally kdb umount was added.

A cmake bug was fixed, so that "make test" now
works immediately.

The kdbconfig.h and kdbos.h received some needed cleaning
up. The HAVE_ macros are now set correctly by cmake, Elektra's
macros are prefixed with KDB_ and not needed macros were
removed.

Some docu was improved and some typos fixed.
Thanks to Erkan Yilmaz <erkan77@gmail.com> for
two fixes.

The test framework now has built-in support for comparing keysets and
keys together with its metadata. Because of usage of macros the
linenumber will be stated correctly for comparison.

Additionally also a script test framework was started.

The documentation was updated and improved at some places.

The build system was improved. Now also ALL, NODEP or DEFAULT are
accepted as magic values for a list of plugins.

Most of the tests run automatically on a buildserver:
http://build.libelektra.org:8080/

It gives you an good impression with how many different compilers elektra
is currently tested. Elektra now also compiles with clang.

You can download the release at:
http://www.markus-raab.org/ftp/elektra/releases/elektra-0.8.3.tar.gz
md5sum: d6b1d668a1a1e155137d4ebc1bd0d5d7
sha256: 48fab82a6b1e8f0038c43ae9ade4da25b697d0aa74e39b7b94056ab7febc4be1

You can install the debian packages for debian squeeze (i386 and amd64 only) by
adding following lines to your /etc/apt/sources.list:
deb     http://build.libelektra.org/debian/ elektra-release-glue main
deb-src http://build.libelektra.org/debian/ elektra-release-glue main

and install all or some of the packages with:
sudo apt-get install libelektra-core4 libelektra-full4 libelektra-bin libelektra-dev libelektra-test libelektra-xmltool4

Have fun!



---------------


0.8.1 Release

The 0.8.1 brings some bugfixes and many small improvements.

The only new feature is that "kdb export" can now be used with any
storage plugin.  This is useful for backups, but also as replacement of
the former "kdb-ls -l" which showed keys and values.
 kdb export <name> simpleini
now does this job. For a full export of system configuration to xml,
you can use:
 kdb export system xmltool

The "kdb get" and "kdb getmeta" tool now support -n and --no-newline to
suppress the newline at the end of the output.

The cmake installation pathes got a overhaul and are now documented
more properly in COMPILE.  Noteworthy is that plugins and include files
can also be installed directly in the system library or include path.
Additionally the build system now handles multiarch systems by supporting
LIB_SUFFIX.

FindPackage of cmake and pkgconfig remain the best methods to use elektra
from external application. New are the examples for such projects in
the folder "external".

The -H and --help support of the kdb tool were still improved, some typos
fixed and some information updated. Additionally the warning output got
some cleanup to be less confusing.

The user experience was improved. The ".kdb" or ".config" directory now
will be created if they do not exist.

The debian packages are now split to have full advantage of the
new plugin system.  The core packages (libelektra, libelektra-dev and
libelektra-bin) do not depend on libxml anymore.  New is the package
libelektra-full, which contains a single dynamic library containing all
plugins statically.  A repository will be announced separately.

There was also some cleanup of old files, like the old highlevel-api
(which does not exist anymore). Finally the API documentation, especially
for plugin developers, was improved.







---------------






0.8.0

Finally it is done, 0.8.0 is finished.
Even though it has a very similar API to 0.7.0
(see APICHANGES) the implementation changed
paradigm-shifting.
0.8.0 introduces a completely new plugin framework,
which allows you to check types and structure
of the keys, notify by dbus, log to syslog,
change the way how filenames are resolved and change
the configuration format and files at runtime.

The C-API, like defined in
 src/include/kdb.h
and also the one to plugins, as defined in
 src/include/kdbplugin.h
is considered to be stable within this 0.8.0 release.


Elektra has a new presence at freedesktop, see
 http://www.freedesktop.org/wiki/Software/Elektra

Download it from:
http://www.markus-raab.org/ftp/elektra/releases/elektra-0.8.0.tar.gz
ftp://www.markus-raab.org/elektra/releases/elektra-0.8.0.tar.gz





---------------






0.7.0

Finally it is done, the 0.7.0 API is finished.
The API covers everything shown in kdb.h and all
releases in 0.7.0 will be 100% ABI and API compatible
supported for at least a year.

Some bugs might break that rule, but there should not be
left many anymore, because every method is at least checked
twice and most have hundred of test cases.
Please report any bugs you find to http://bugs.libelektra.org
- portability and build bugs
- mistakes, misspelling in documentation

This is not valid for kdb-tools, there are lots of bugs
remaining there and a rewrite would be very useful.

For that reason there will be a separate release for
kdb-tools, libelektratools and the backend interface.

The stable APIs of libelektratools and the backend interface
will be announced separately, but they will be part of 0.7.x.

kdb-tool and kdbedit and so will also have separate
releases, but because they are not a library there
is no API or ABI issue there.

Pages related to 0.7.0:
http://www.libelektra.org/GetStartedMounting
http://www.libelektra.org/Tutorial
http://www.libelektra.org/API-Changes0.7
http://www.libelektra.org/Changes0.7

Marketing issues:
- Freshmeat announcment
- Sourceforge upload
- API docu upload
- forums
- newsgroups
- other mailinglists

Download it from:
http://www.markus-raab.org/ftp/elektra-0.7.0.tar.gz
ftp://www.markus-raab.org/elektra-0.7.0.tar.gz







---------------




0.7.0rc5 The last release candidate before 0.7.0

I am proud to present the finished  0.7.0 external API in
kdb.h [0].

A lot changed to 0.6.10 but the fundamentals are clearer then
ever before: 3 Classes with their methods, a clear, easy and
mature API. It is very hard to use it wrong but you get
best results with very short code:

http://www.libelektra.org/Tutorial

Other parts are not finished, like libelektratools, the
kdb-tool and the backend interface. They are not part of
the official 0.7 release and get their own release later.

The reasons to release within the next week are:
 - The API is finished
 - Oyranos needs a stable release from elektra
 - the last release is long ago
 - unstable has been much more stable than stable since some time
 - there have been no commits to the stable branch since
   unstable existed
 - Elektra grows just too complex to wait for all parts to finish

Now some highlights from one month of work from 333 ChangeLog
lines:
 - many many bugs were fixed
 - the core completely compiles with -ansi -pedantic -std=c99
 - all code compiles with -Wall -Werror
 - the test-suite is now complete
 - much cleaner infrastructure within the code
 - many autoconf/automake issues fixed
 - static, dynamic building, linking with defaults backends
   everything works!
 - many new options to stat and remove all keys and so on..
 - environment variables and passwd database allow subtle
   influence of where the user configuration resides
 - kdb info now prints where configuration lies on hard disc




---------------




0.7.0rc4 - 12.05.2008
I am proud to present the fourth release candidate of elektra
0.7.0.

It is now possible to enable and disable Backends and Bindings
with configure switches. The documentation was improved.
The c++ binding now basically works, see the tests for examples.
Ini and Berkleydb backends now compile again.
There were a lot of bug fixes, like mntent.h fix for Mac OS X.




---------------




0.7.0rc3 - 24.04.2008
Call for 0.7 finish

Unfortunately there is not much response about the 0.7 release
candidates but there is still a lot to do to have a mature
library worth to be announced as stable.

Specifically there are following things to do and we really need
help to achieve that faster than end of this year:

There are now only 3 backends working well tested but undistinguished
in handling error scenarios. At least 3 more Backends are needed
also tested in real world applications. Favoured are existing but
not ported backends like berkleydb, ini, winregistry and uniconf.

Working bindings have a lot meaning in how complete and useful
the API is. It would be preferable to have at least 3 bindings
working with at least an example application like kdb-tool.
They could be cpp, python and one open.

A lot of bug squashing is left open. Please don't hesitate to open
and fix more bugs on http://bugs.libelektra.org.

Documentation is not yet completely updated to 0.7. Especially the
homepage has a lot of outdated information, but also the API documentation
needs another eye on it and the tutorial needs a rewrite.

There might be some licencing issues in some parts of elektra,
everything should be BSD, if you find something please open a bug.

A partial blocker is the kdb-tool, where a rewrite would be nice but
is not necessary for 0.7 (the library counts not the environment).

Other things like portability are taken seriously but are not blockers,
they can be fixed later too.

What is really good working in elektra is the infrastructure like
svn, homepage, bug tracking, many thanks to José Luis Tallón for
that!

Now some words about 0.7.0rc3:

Version macros were added to have static information about
the elektra version compiled against. There were many
enhancements in the C++ Binding, the Key Class is now
quite well complete.

While writing the C++ binding I realized that there is need
for keyInc() the opposite of keyDel() to increase the
viability of a key object.

keyVNew() and ksVNew() were added to make it possible to pass
the va_list from the C++ binding.

The blocker bug that libelektra assumes system/elektra/mountpoints
to exist was solved, many thanks to Patrick Sabin. The tests
now work without any preloading without memory leaks.

And lots of bug fixes (Many thanks to Kai-Uwe Behrmann):
- sed changed for macosx compatibility
- fixed signature ssize_t ksToStream
- also set CXXFlags
- fix wrong parent at end in ksToStream
- fix return value of successful kdb import
- string is default type for xml without type=""
- fix extra_dist some missing files
- remove , at end of enumeration lists

The svn id is unstable@1352:
https://svn.libelektra.org/svn/elektra/unstable/

The release candidate is available under:
http://www.markus-raab.org/ftp/elektra-0.7.0rc3.tar.gz



---------------



0.7.0rc2 - 23.03.2008
Second Release Candidate of unstable repository.

We proudly present the second release candidate for Elektra 0.7.0. Many
bugs were fixed, kdbGet() and kdbSet() is now really stable, useful and
well documentated. test_kdb also tests hosts and fstab next to filesys
with 6598 test cases.

kdbSet() now supports a parent key to only set a part of the
configuration passed by the keyset, allowing to e.g. save system and
user configuration seperately. It only calls kdbSet_backend() when it
is actually necessary. The splitting works much more efficient with n*b
instead of n^2*b.

The highlevel functions kdbGetKey() and kdbSetKey() now work well with
Capabilities. This allows very easy changing of keys inside backends
even with some lacking capabilities, see GetStartedMounting.

Patrick Sabin and I wrote our bachelor thesis about mounting backends for
configuration. It gives a detailed introduction in problem and choices
and the actual implementation.
See: doc/elektra.pdf doc/elektra.ps.gz





---------------




0.7.0rc1 - 06.12.2007
First Release Candidate of unstable repository.
= Large Changes =

== Multiple Backends ==

Elektra now supports multiple backends at once. That means every path
like user/sw or system/filesystems can reside in a different backend.

Use cases for backend mounting: 1. There are many configurationfiles lying
on every system that can't be replaced for various reasons. These files
can be fade in into the global elektra namespace without any notice by
applications using these files. Backends for /etc/fstab, /etc/mtab and
/etc/passwd exist right now.

2. Users or administrators might get used by ordained files or syntax
without wanting to change the whole configuration using elektra. The
mounting technique allows them to choose.

3. Specific programs may have very complex and large configuration. Binary
files with index may give them a fairly better performance without
missing the connection to the global namespace.

4. Configuration provided by network or local daemons for notification
and caching can't be used for every program. Configuration related to
bootup or some system users need to be available without them, but should
also be accessed by applications needing configuration from network.

If you are not convinced - Its just about choice and you can go on using
only one backend.

See http://www.libelektra.org/Backends what is and might be possible.

== kdbOpen(), kdbClose(), kdbGet() and kdbSet() ==

These 4 core functions do now everything related to backend
communication. All the other functions are build upon it. This makes
backend writing much easier. Removing and Stat works by setting a flag
in a key.

KDB * is now a typedef for the _KDB structure and not a
void* pointer.

== Keyset ==

Starting with ksNew you can give a hint how large the keyset may grow. You
can also give a list of keys which should be added at startup.

ksGenerate() makes use out of that and generates you a keyset in C-Code
which you can use in your applications or for regression testing.

The internal structure of keyset is now a growing (and shrinking)
array. Keys may belong to multiple keysets now. The last ksDel() deletes
the key automatically (reference counter).

This implementation is a lot faster, the benchmark is for 0.7.0rc1 better
then for 0.6.*, even though the mounting logic takes a bit of performance,
but very little and something about zero when no backend is mounted.

== Key ==

The key now uses sizes for name, owner, value and comment. This allows a
complete new technique of serializing: keyCompose(). With that you can
serialise a key without a single malloc(). keySerialize() is rewritten
for the new key struct too.

== Access Types ==

The directory is now marked by the executable flags. That means you can
disallow other users or groups to list your keys.

== Capabilites ==

Some backends fullfill the whole specification of kdbGet_template() and
kdbSet_template() and can be used by any program for any purpose. Other
backends have principle limitations and do not fullfill the specification,
but can do more than enough to be useful.

To handle this problem we created a data structure describing what
capabilites a backend does not have. With that technique you can use the
testing framework from early steps developing the backend on. To do so,
just declare your backend can do nothing and delete step by step while
your backend evolves.

== Testing framework ==

There is now a large testing framework with 11 collections with up to
8128 tests each. Most conditions of the whole sourcecode are checked,
that means if you random change something in the code you have a good
chance that a test case will fail.

There are 2 new flags:
 --enable-valgrind-tests
   Allows you to enable valgrind for testing, a full list of memory
   leaks is printed after every test.
 --enable-gcov
   Use it to see what lines of code are covered by tests.

The testcases are without any memory leak (dozens of leaks are fixed,
especially in xml code).

---------------

0.6.10 - 10.03.2007 Elektra received some stability updated from Patrice
Dumas.

---------------

0.6.6 - 21.12.2006 support for directories with values and comments on
filesys, berkeleydb and daemon backends.

---------------

0.6.4 - 08.09.2006 Tons of improvements and standarizations to the build
system from Patrice Dumas and Yannick.  Many improvements to the daemon
backend from Yannick.  Several other bug fixes.  We are getting closer
to a production daemon.

---------------

0.6.2 - 02.06.2006 - Includes more robust key name handling and
intelligent duplicate "/" removal - Escaping of "/" on key names is
now supported - Better automatic UTF-8 conversions - More tunnings for
better future daemon backend support - More robust berkeleydb backend -
Backends are now installed on /lib/elektra/ - The build system was tunned
to be included as a Fedora Extras package

---------------

0.6.0 - 30.03.2006 Public release of the new API.  Check it out in SF.
