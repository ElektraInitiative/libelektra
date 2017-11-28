# Compile

## Dependencies

For the base system you only need cmake and build-essential (make, gcc,
some Unix tools):

	sudo apt-get install cmake build-essential

Or on RPM based systems (CentOS):

	sudo yum install -y cmake3 gcc-c++

Or on macOS Sierra, most of the build tools can be obtained by installing Xcode (from the App Store). Other required tools may be installed using [brew](http://brew.sh/). First install brew as described on their website. Then issue the following command to get cmake to complete the basic requirements:

	brew install cmake


## Quick Guide

Run the following commands to compile Elektra with non-experimental
parts where your system happens to fulfil the dependences (continue
reading the rest of the document for details about these steps):

```
git clone https://github.com/ElektraInitiative/libelektra.git
cd libelektra
mkdir build
cd build
cmake ..  # watch output to see if everything needed is included
ccmake .. # optional: provides a console based GUI to give an overview of the available compilation options and settings
make -j 5
make run_nokdbtests  # optional: run tests
```

The last line only runs tests not writing into your system.
See [TESTING](/doc/TESTING.md) for how to run more tests.
Afterwards you can use `sudo make install && sudo ldconfig` to install Elektra.
See [INSTALL](/doc/INSTALL.md) for more information about
installation of self-compiled Elektra (such as how to uninstall it).


## Optional Dependences

> Note: You do not need to install the dependencies listed here.
> If they are not available, some of the functionality gets disabled automatically.
> The core of Elektra never depends on other libraries.

To build documentation you need doxygen (we recommend 1.8.8+), graphviz and [ronn](https://github.com/rtomayko/ronn/blob/master/INSTALLING#files):

	apt-get install doxygen graphviz ronn

Or on RPM based systems:

	sudo yum install -y doxygen docbook-style-xsl graphviz ruby
	gem install ronn

Or on macOS Sierra using brew:

	brew install doxygen graphviz
	brew install ruby (in case ruby is not already installed)
	gem install ronn

To build PDF documentation you need `pdflatex` with

	apt-get install pdflatex texlive-fonts-recommended texlive-latex-recommended texlive-latex-extra

For the plugins, please refer to the README.md of the respective plugin.
For example, for CentOS:

	sudo yum install -y boost-devel libdb-devel GConf2-devel libxml2-devel yajl-devel   \
	libcurl-devel augeas-devel libgit2-devel lua-devel swig python34-devel python-devel \
	java-1.8.0-openjdk-devel jna ruby-devel byacc

For the Debian package, please refer to debian/control (in the debian
branch).

## Preparation

Elektra uses cmake.
Tested are cmake version 2.8.9 (minimum) and version 3.0.2 (recommended) among others.

To configure Elektra graphically (with curses) run (`..` belongs to command):

    mkdir build && cd build && ccmake ..

and press 'c' to configure the cache (might be necessary multiple times, and once on the first time in case you don‘t see any settings).
After applying the desired settings, press 'g' to generate the make file.


All options described here, can also be used with cmake rather than
ccmake (`..` does also here belong to the command):

    mkdir build && cd build && cmake -D<OPTION1>=<VAR1> -D<OPTION2>=<VAR2> ..

For information what you can use as `OPTION1` and `OPTION2`, see above.
Note: You have to enclose a value with quotes `""` if it contains a semicolon (`;`).
E.g.:

    cmake -DPLUGINS="dump;resolver;yajl" ..

Some scripts in the folder of the same name may help you running cmake.

### Compilers

For supported compilers have a look at the automatic build farm on
https://build.libelektra.org/


|   Compiler        |         Version             |      Target       |
|-------------------|-----------------------------|-------------------|
|      gcc          | gcc (Debian 6.3.0-18) 6.3.0 |      amd64        |
|      gcc          | gcc (Debian 4.7.2-5) 4.7.2  |      i386         |
|      gcc          | gcc (Debian 4.7.2-5) 4.7.2  |      amd64        |
|      gcc          | gcc 4.8                     |      amd64        |
|      gcc          | gcc 4.9                     |      amd64        |
|      gcc          | (Debian 4.4.5-8) 4.4.5      |      amd64        |
|      gcc          | (Debian 4.4.5-8) 4.3        |      amd64        |
|      gcc          | 4.6                         |      armhf        |
|      mingw        | 4.6                         |      i386         |
|      clang        | version 3.5.0-1~exp1        |x86_64-pc-linux-gnu|
|      clang        | 3.9.0                       |x86_64-pc-linux-gnu|
|      clang        | 8.1.0                       |      macOS        |
|      icc          | 14.0.2 20140120             |x86_64-pc-linux-gnu|
|      gcc/g++      |                             | openbsd 4.9.3 (*) |

> (*) OpenBSD ships an old version of GCC per default, which can not compile Elektra.
> A manual installation of egcc/eg++ is required. Note that not every OpenBSD
> mirror provides the eg++ package. Elektra builds are confirmed with
> egcc/eg++ 4.9.3 in OpenBSD 5.9.
> The packages are called gcc and g++.
> Compile with `CC=/usr/local/bin/egcc CXX=/usr/local/bin/eg++`.

To change the compiler, use cmake settings `CMAKE_C_COMPILER` and `CMAKE_CXX_COMPILER`.

To use gcc-4.3 for example

    cmake -DCMAKE_C_COMPILER=gcc-4.3 -DCMAKE_CXX_COMPILER=g++-4.3 ..

To change the compiler with ccmake, you may need to toggle advanced options (key 't').

### Options

Some options, i.e. `PLUGINS`, `BINDINGS` and `TOOLS` are either:

- a list of elements separated with a semicolon (`;`)
   (note that shells typically need `;` to be escaped)
- a special uppercase element that gets replaced by a list of elements, that are:
  - `ALL` to include all elements (except elements with unfulfilled dependencies)
  - `NODEP` to include all elements without dependencies
- elements prefixed with a minus symbol (`-`) to exclude an element

Examples for this are especially in the subsection `PLUGINS` below, but they work in the
same fashion for `BINDINGS` and `TOOLS`.

#### Plugins

Read about available plugins [here](/src/plugins/).

Because the core of elektra is minimal, plugins are needed to
actually read and write to configuration files (_storage plugins_),
commit the changes (_resolver plugins_, also takes care about how
the configuration files are named) and also do many other
tasks related to configuration.

The minimal set of plugins you should add:
- [dump](/src/plugins/dump) is the default storage.
  If you remove it, make sure you add another one and set
  `KDB_DEFAULT_STORAGE` to it.
- [resolver](/src/plugins/resolver) is the default resolver.
  If you remove it, make sure you add another one and set
  `KDB_DEFAULT_RESOLVER` to it.
- [sync](/src/plugins/sync) is very useful to not lose any data.
  If you do not want to include it, make sure to set
  `/sw/elektra/kdb/#0/current/plugins` to a value not containing sync
  (e.g. an empty value).
  See [kdb-mount(1)](/doc/help/kdb-mount.md).

By default nearly all plugins are added. Only experimental plugins
will be omitted:

    -DPLUGINS="ALL;-EXPERIMENTAL"

To add also experimental plugins, you can use:

    -DPLUGINS=ALL

> Note that plugins get dropped automatically if dependences are not satisfied.

To add all plugins except some plugins you can use:

    -DPLUGINS="ALL;-plugin1;-plugin2"

For example, if you want all plugins except the jni plugin you would use:

    -DPLUGINS="ALL;-jni"

To add all plugins not having additional dependencies
(they need only POSIX), you can use

    -DPLUGINS=NODEP

Note, that every `infos/provides` and `infos/status` field written uppercase can
be used to select plugins that way (see README of [individual plugins](/src/plugins)).
You also can combine any of these fields
and add/remove other plugins to/from it, e.g. to include all plugins without deps,
that provide storage (except `yajl`) and are maintained, but not include all plugins
that are experimental, you would use:

    -DPLUGINS="NODEP;STORAGE;-yajl;MAINTAINED;-EXPERIMENTAL"

The inclusion is determined by following preferences:

1. if the plugin is explicit excluded with `-plugin`
2. if the plugin is explicit included with `plugin`
3. if the plugin is excluded via a category `-CATEGORY`
4. if the plugin is included via a category `CATEGORY`
5. plugins are excluded if they are not mentioned at all
   (neither by category nor by name)

Note, that changing `PLUGINS` will not modify the defaults used
after Elektra was installed.  For this endeavour you need to change:

    -DKDB_DEFAULT_RESOLVER=resolver

and

    -DKDB_DEFAULT_STORAGE=dump

The default resolver and storage will write to `KDB_DB_FILE` and `KDB_DB_INIT`
([for bootstrapping](/doc/help/elektra-bootstrapping.md)).

Obviously, you can pass the exact list of plugins you want, e.g.:

    -DPLUGINS="resolver;sync;dump"

Some plugins are compile-time configurable. Then you can choose which
features are compiled in or out. This is especially important in the
bootstrapping phase, because then only the compiled in configuration
applies. To compile-time-configure a plugin, you just pass a underscore
(`_`) and flags after the name of the plugin.

The resolver for example distinguish between 3 different kind of flags:

    -DPLUGINS="resolver_baseflags_userflags_systemflags"

Following baseflags are available:

- `c` for debugging conflicts
- `f` for enabling file locking
- `m` for enabling mutex locking

The user flags are (the order matters!):

- `p` use passwd/ldap to lookup home directory using `getpwuid_r`
- `h` use the environment variable HOME
- `u` use the environment variable USER
- `b` use the built-in default cmake variable `KDB_DB_HOME`

The system flags are (the order matters!):

- `x` use the environment variable `XDG_CONFIG_DIRS`
  (`:` are interpreted as part of filename, no searching is done!)
  This option is not recommended (unless for testing), because it
  allows users to fake system configuration.
- `b` use the built-in default cmake variable `KDB_DB_SYSTEM`
- note: if a path that begins with a slash (`/`) is chosen, the system flags are irrelevant
  and the path is taken as-is.

For example, one may use:

    -DPLUGINS="resolver_lm_uhpb_b"

To add `resolver_l_h_b` you need to specify

    -DPLUGINS="resolver;resolver_l_h_b"

You can add resolver with any combination of the flags, even if they are
not available in `ALL`.

#### Tools

Tools are used to add extra functionality to Elektra.
The flag used to specify which tools are compiled is
`-DTOOLS`, thus flag works similarly to the `-DPLUGINS` flag,
but is more limited in its functionality (which does not
matter, because there are not so many tools).

To add all non-experimental tools, you can use::

    -DTOOLS=ALL

> Note that the behavior is different to PLUGINS
> which includes all PLUGINS if ALL is used.

To add all tools except of race, you can use:

    -DTOOLS="ALL;-race"

To specify specific tools you can use, e.g.:

    -DTOOLS=qt-gui;kdb


#### Bindings

Bindings are used in the same way as `TOOLS`.
For example, to include all non-experimental bindings you can use:

    -DBINDINGS=ALL

> Note that the behavior is different to PLUGINS
> which includes all PLUGINS if ALL is used.

Note that the same languages are sometimes available over GI and SWIG.
In this case, the SWIG bindings are preferred.
The SWIG executable may be specified with:

    -DSWIG_EXECUTABLE=/usr/bin/swig3.0

If this option is not used, cmake will find the first occurrence of
`swig` in your environment's path.
Even with `ALL` GI bindings (deprecated) and gsettings (experimental) are not included.
To include them, use:

    -DBINDINGS="ALL;GI;gsettings"

Some bindings provide different APIs (and not a different language), e.g:

- `gsettings`
- `INTERCEPT` with `intercept_fs` and `intercept_env`

To not add such APIs, but only `swig` bindings and `cpp`, you can use:

    -DBINDINGS="SWIG;cpp"


#### CMAKE_BUILD_TYPE

`Debug`, `Release` or `RelWithDebInfo`
See help bar at bottom of ccmake for that option or:
http://www.cmake.org/Wiki/CMake_Useful_Variables


### BUILD_SHARED BUILD_FULL BUILD_STATIC

`BUILD_SHARED` is the typical build you want to have on systems that support `dlopen`.
It can be used for desktop builds, but also embedded systems as long as they support
`dlopen`, for example, `BUILD_SHARED` is used on OpenWRT with musl.
Using `BUILD_SHARED` every plugin is its own shared object.

`BUILD_FULL` links together all parts of Elektra as a single shared `.so` library.
This is ideal if shared libraries are available, but you want to avoid `dlopen`.
Some tests only work with `BUILD_FULL`, so you might turn it on to get full
coverage.

`BUILD_STATIC` also links together all parts but as static `.a` library.
It is only useful for systems without `dlopen` or if the overhead of
`dlopen` needs to be avoided.

All three forms of builds can be intermixed freely.

For example, to enable shared and full build, but disable static build,
one would use:

    cmake -DBUILD_SHARED=ON -DBUILD_FULL=ON -DBUILD_STATIC=OFF ..

#### ELEKTRA_DEBUG_BUILD and ELEKTRA_VERBOSE_BUILD

Only needed by Elektra developers.
Make the library to output logging information.
It is not recommended to use these options.

#### BUILD_DOCUMENTATION

Build documentation with doxygen (API) and ronn (man pages).

#### Developer Options

As developer you should enable `ENABLE_DEBUG` and `ENABLE_LOGGER`.
By default no logging will take place, see [CODING](/doc/CODING.md)
for information about logging.

Then continue reading [testing](/doc/TESTING.md) for options about
testing.

#### CMAKE_INSTALL_PREFIX

`CMAKE_INSTALL_PREFIX` defaults to `/usr/local`.
So by default most files will installed below `/usr/local`.
Exceptions to this are files handled by [INSTALL_SYSTEM_FILES](#install_system_files).

Edit that cache entry to change that behavior.
Also called system prefix within the documentation.

If you want to create a package afterwards it is ok to use
paths that you can write to (e.g. `-DCMAKE_INSTALL_PREFIX=/home/username/`)

#### LIB_SUFFIX

Lets you install libraries into architecture specific folder.
E.g. for 32/64 bit systems you might install libraries under
`lib64`. Set `LIB_SUFFIX` to `64` to achieve exactly that.
So the system library folder will be `CMAKE_INSTALL_PREFIX/lib64`
then.

#### TARGET_INCLUDE_FOLDER

By default include folders will be installed below
`CMAKE_INSTALL_PREFIX/include/elektra`.
This entry let you change the elektra.
If the entry is empty, the include files will be
installed directly to `CMAKE_INSTALL_PREFIX/include`.

#### TARGET_PLUGIN_FOLDER

Similar to above, but with the plugins. Default is:
`CMAKE_INSTALL_PREFIX/lib${LIB_SUFFIX}/elektra`
It can be also left empty to install plugins next
to other libraries.

#### GTEST_ROOT

Specifies the root of the GoogleTest sources, to be used
for some of the tests. A `CMakeLists.txt` inside `GTEST_ROOT`
will be searched as way to detect a valid GoogleTest source
directory.
If it is empty (`""`), an internal version of gtest will be used.

It is recommended that you browse through all of the options using ccmake.
Afterwards press 'c' again (maybe multiple times until all variables are
resolved) and then 'g' to generate.  Finally press 'e' to exit.

#### INSTALL_BUILD_TOOLS

Specifies that the build tools, i.e. `elektra-export-symbols` and `elektra-export-symbols`
are installed (by default off). Is needed for cross-compilation.

#### INSTALL_SYSTEM_FILES

Some of Elektra’s targets require to be installed into specific folders in the
file system hierarchy to work properly.

This variable is enabled by default but requires the install target to have the
rights to write into the corresponding folders. Set `-DINSTALL_SYSTEM_FILES=OFF`
if you do not need any of them.

If you do not have root rights you can copy them manually to your user folder.

Currently the installed system files are as following:

|   Module        |         Description             |      Install Path                      |
|-----------------|---------------------------------|----------------------------------------|
| bash-completion | bash tab auto completion file   | `completionsdir` from pkg-config (*)   |
| zsh-completion  | zsh tab auto completion file    | /usr/share/zsh/vendor-completions      |
| GIR             | introspection file for bindings | `INTROSPECTION_GIRDIR` from pkg-config |
| GSettings       | GSettings backend module        | `GIO_MODULE_DIR` from pkg-config       |

> (*) Or `/usr/share/bash-completion/completions` as fallback.

#### ENABLE_OPTIMIZATIONS

In order to keep the binaries as small as possible this flag allows to trade memory for speed.

## Building

### Without IDE

To build the source use:

    make

You can pass:
- `-j` for parallel builds
- `VERBOSE=1` to see the invocations of the compiler

Continue by reading [INSTALL](INSTALL.md).

### With CodeBlocks

You can build Elektra using Code::Blocks under Gentoo:

Precondition:
Make sure you have a compiler, xml2 (for kdb tool) and xsl (see later) installed.
cmake configure will help you with that, it will make sure you don't forget something
essential.

For Most Linux system all you have to do is open up a console and

    mkdir build
    cd build
    cmake .. -G 'CodeBlocks - Unix Makefiles'
    make package

Note  1:
    You can use other editor if you like just type cmake at the
    console to get a list of option you can pass to cmake as long as well
    as a list of what code editor project cmake can create.

Note 2:
    For Unix if you have nCurses install you can run ccmake to set important option after
    running cmake like to enable debug symbol.

Note 3:
    for Gentoo is recommend to emerge sys-apps/lsb-release to name the package
    right even thou not required.


## Maintainer's Guide

### Multiarch

On multiarch (multiple architectures installed in one system), you need to set `LIB_SUFFIX`.
For example, if you want to have the binaries in `lib64` and `lib32`, you
would use for the libraries to be installed in `${CMAKE_INSTALL_PREFIX}/lib64`:

	-DLIB_SUFFIX="64"

If there is a directory for different architectures, simply prepend an `/`.
For example, for Debian:

	-DLIB_SUFFIX="/$(DEB_HOST_MULTIARCH)"




### RPATH

By default Elektra uses `RPATH` to hide its plugins. This makes it obvious that
external applications should *not* link against plugins. Instead every application
should use the `elektraModulesLoad()` API to load Elektra’s modules.

The folder where the plugins are located is a subdirectory of where the
libraries are installed. The name of the subdirectory can be specified
using `TARGET_PLUGIN_FOLDER` and is `elektra` by default. You might
want to encode Elektra’s `SOVERSION` into the folders name, if you want
different major versions of Elektra be co-installable.

Elektra’s use case for `RPATH` is considered acceptable, so we recommend to use it
because:

- plugins do not clutter the library folder nor the `ld.so.cache`
- it works well with multiarch (`LIB_SUFFIX` is also honored for plugins)
- which plugins are used should be decided at mount-time and be globally available in the
  same way for every application. `RPATH` supports exactly that because it even overrides
  `LD_LIBRARY_PATH`.

Unfortunately, there are also drawbacks:

- it makes Elektra non-relocatable (`RPATH` is decided at compile-time, so you cannot
  simply move Elektra’s installations within the file system (e.g. from `/usr/local` to `/usr`)
- it requires modern `ld.so` implementations that honor `RPATH` from libraries.
  This is the case for most `libc` implementations including Linux and macOS, but not
  for, e.g., `musl`.


If you want Elektra to *not* use `RPATH`, you can add:

	-DTARGET_PLUGIN_FOLDER="" -DCMAKE_SKIP_INSTALL_RPATH=ON

Then all plugins are directly installed to the library directory and loaded
like other libraries (in any of `ld.so` paths).

Alternatively, which gives you the advantage not to clutter the main library path,
is to add the plugin folder in `/etc/ld.so.conf.d/elektra`. Note that it still allows
applications to link against plugins.


## Troubleshooting

### Missing Links/Libraries

If you get errors that `libelektra-resolver.so` or `libelektra-storage.so` are missing,
or the links do not work, you can use as workaround:

	cmake -DBUILD_SHARED=OFF -DBUILD_FULL=ON ..

This issue was reported for:

- OpenSuse 42 (when running `make run_all`)
- CLion IDE (does not allow to build)


### Dependencies not Available for Cent OS

Please enable EPEL https://fedoraproject.org/wiki/EPEL

	# Install EPEL for RHEL 7
	curl -o epel-release-7-8.noarch.rpm \
	  http://dl.fedoraproject.org/pub/epel/7/x86_64/e/epel-release-7-8.noarch.rpm
	sudo rpm -ivh epel-release-7-8.noarch.rpm
	sudo yum update

For Bindings swig3 is recommended. swig2 only works on some distributions.
E.g., for Debian Jessie the bindings will crash.

At time of writing, no swig 3 was available, not even in EPEL.
Thus you need to install swig3 manually:

	curl https://codeload.github.com/swig/swig/tar.gz/rel-3.0.10 | tar xz
	cd swig-rel-3.0.10 && ./autogen.sh && ./configure && make
	sudo make install
	cd ..

Also, no ronn was available, thus you need to do:

	gem install ronn


### Cross Compiling

In Elektra cross compiling needs two steps.  If you get errors like
`elektra-export-errors_EXE_LOC` not found, go on reading.

In the first step, you need to compile Elektra for the host architecture
and install the build tools:

	cmake -DINSTALL_BUILD_TOOLS=ON \
	      -DINSTALL_SYSTEM_FILES=OFF \
	      -DCMAKE_PREFIX_PATH=$(STAGING_DIR_HOST) \
	      ..
	make -j 5
	make install -j 5

Where `$(STAGING_DIR_HOST)` must be a directory to be found in the later
build process.  In particular, `$(STAGING_DIR_HOST)/bin` must be in a
directory found by a later `find_program`.

Then you need to compile Elektra again, but for the target architecture.
Now, the build tools such as `elektra-export-errors` should be found in
the `$(STAGING_DIR_HOST)/bin` where they were installed before.

For reference, you can look into the
[OpenWRT Elektra Makefile](https://github.com/openwrt/packages/blob/master/libs/elektra/Makefile)
and the
[CMake in OpenWRT](https://github.com/openwrt/openwrt/blob/master/include/cmake.mk).

## See also

- [INSTALL](INSTALL.md).
- [TESTING](TESTING.md).
