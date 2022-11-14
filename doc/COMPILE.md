# Compile

## Dependencies

For the base system you only need [cmake3](https://cmake.org/cmake/help/v3.0/),
[Git](https://git-scm.com/), a C99 compiler and essential build tools
(make and some standard Unix tools; alternatively ninja and
clang are also supported but not described here). Those can be installed as follows:

- on APT-based systems (Ubuntu, Debian):

  ```sh
  sudo apt-get install cmake git build-essential
  ```

- on RPM-based systems (CentOS, Red Hat Enterprise Linux, Oracle Linux):

  ```sh
  sudo yum install -y cmake git gcc-c++ make
  ```

- on macOS, most of the build tools can be obtained by installing Xcode (from the App Store). Other required tools may be installed using [brew](http://brew.sh/). First install brew as described on their website. Then issue the following command to get cmake and Git in order to complete the basic requirements:

  ```sh
  brew install cmake git
  ```

## Quick Guide

Run the following commands to compile Elektra with non-experimental
parts where your system happens to fulfill the dependencies (continue
reading the rest of the document for details about these steps):

```sh
git clone https://github.com/ElektraInitiative/libelektra.git
cd libelektra
mkdir build
cd build
cmake ..  # watch output to see if everything needed is included
ccmake .. # optional: overview of the available build settings (needs cmake-curses-gui)
cmake --build . -- -j5
cmake --build . --target run_nokdbtests # optional: run tests
```

The last line only runs tests without writing onto your system.
See [TESTING](/doc/TESTING.md) for how to run more tests.
Afterwards you can use `sudo make install && sudo ldconfig` to install Elektra.
See [INSTALL](/doc/INSTALL.md) for more information about
installation of self-compiled Elektra (such as how to uninstall it).

## Optional Dependencies

> Note: You do not need to install the dependencies listed here.
> If they are not available, some of the functionality gets disabled automatically.
> The core of Elektra never depends on other libraries.

### Documentation dependencies

To build the documentation you need doxygen (we recommend 1.8.8+), graphviz and [ronn-ng](https://github.com/apjanke/ronn-ng/blob/master/INSTALLING.md). These can be installed as follows:

- on APT-based systems (Ubuntu, Debian):

  ```sh
  apt-get install doxygen graphviz
  gem install ronn-ng -v 0.10.1.pre1
  ```

- on RPM-based systems (CentOS, Red Hat Enterprise Linux, Oracle Linux):

  ```sh
  sudo yum install -y doxygen docbook-style-xsl graphviz ruby
  gem install ronn-ng -v 0.10.1.pre1
  ```

- on macOS using brew:

  ```sh
  brew install doxygen graphviz
  brew install ruby # in case ruby is not already installed
  gem install ronn-ng -v 0.10.1.pre1
  ```

To build PDF documentation you need `pdflatex`.
You can install it as follows:

- on APT-based systems (Ubuntu, Debian):

  ```sh
  apt-get install \
    texlive-latex-base \
    texlive-latex-recommended \
    texlive-latex-extra \
    texlive-fonts-recommended \
    texlive-fonts-extra \
    texlive-science
  ```

### Plugin dependencies

For dependencies of plugins, please refer to the [README.md](https://www.libelektra.org/plugins/readme) of the respective plugin.

A small subset of build dependencies to get you started:

- on RPM-based systems (CentOS, Red Hat Enterprise Linux, Oracle Linux):

  ```sh
  sudo yum install -y libdb-devel GConf2-devel libxml2-devel yajl-devel   \
  libcurl-devel augeas-devel libgit2-devel lua-devel swig python34-devel python-devel \
  java-1.8.0-openjdk-devel jna ruby-devel byacc
  ```

- on APT-based systems (Ubuntu, Debian):

  ```sh
  sudo apt install -y libxerces-c-dev libxml2-dev libyajl-dev \
  libcurl4-gnutls-dev libaugeas-dev git git-buildpackage dh-lua liblua5.2-dev \
  dh-python python3-all python3-dev default-jdk libjna-java ruby-dev flex bison
  ```

## Preparation

Elektra uses [cmake3](https://cmake.org/cmake/help/v3.0/).
Tested are cmake version 3.0.2 and 3.7.2 among others.

To configure Elektra graphically (with curses) run (`..` belongs to command):

```sh
mkdir build && cd build && ccmake ..
```

and press `c` to configure the cache (might be necessary multiple times, and once on the first time in case you don‘t see any settings).
After applying the desired settings, press `g` to generate the make file.

All options described here, can also be used with `cmake` rather than
`ccmake` (`..` does also here belong to the command):

```sh
mkdir build && cd build && cmake -D<OPTION1>=<VAR1> -D<OPTION2>=<VAR2> ..
```

For information what you can use as `OPTION1` and `OPTION2`, see above.
Note: You have to enclose a value with quotes `""` if it contains a semicolon (`;`).
E.g.:

```sh
cmake -DPLUGINS="dump;resolver;yajl;list;spec" ..
```

Some scripts in the folder of the same name may help you running cmake.

### Compilers

You should be able to compile Elektra with any C99 compiler.
For a list of compilers we test with have a look at:

- our [Docker containers](/scripts/docker) orchestrated
  by our [Jenkinsfile](/scripts/jenkins/Jenkinsfile) being built
  on [our build server](https://build.libelektra.org/)
- [Cirrus](/.cirrus.yml)
- [GitHub Actions](/.github/workflows)

Here is an additional list of compilers used by developers (for build servers, see links above):

| Compiler | Version                                          | Target                    |
| -------- | ------------------------------------------------ | ------------------------- |
| gcc      | gcc (Debian 8.3.0-6) 8.3.0                       | x86_64-linux-gnu          |
| gcc      | gcc (Debian 10.2.1-6) 10.2.1 20210110            | x86_64-linux-gnu          |
| gcc      | gcc (GCC) 11.2.1 20220127 (Red Hat 11.2.1-9)     | x86_64-redhat-linux       |
| gcc      | gcc (GCC) 12.2.1 20220819 (Red Hat 12.2.1-2)     | x86_64-redhat-linux       |
| gcc      | gcc-12 (Homebrew GCC 12.2.0) 12.2.0              | x86_64-apple-darwin21     |
| gcc      | Homebrew clang version 15.0.1                    | x86_64-apple-darwin21.6.0 |
| clang    | clang version 14.0.5 (Fedora 14.0.5-1.fc36)      | x86_64-redhat-linux-gnu   |
| clang    | Apple clang version 14.0.0 (clang-1400.0.29.102) | x86_64-apple-darwin21.6.0 |

> (¹) OpenBSD ships an old version of GCC per default, which can not compile Elektra.
> A manual installation of egcc/eg++ is required. Note that not every OpenBSD
> mirror provides the eg++ package. Elektra builds are confirmed with
> egcc/eg++ 4.9.4 in OpenBSD 6.3.
> The packages are called gcc and g++.
> Compile with `CC=/usr/local/bin/egcc CXX=/usr/local/bin/eg++`.

To change the compiler, use cmake settings `CMAKE_C_COMPILER` and `CMAKE_CXX_COMPILER`.

To use gcc-4.3 for example

```sh
cmake -DCMAKE_C_COMPILER=gcc-4.3 -DCMAKE_CXX_COMPILER=g++-4.3 ..
```

To change the compiler with `ccmake`, you may need to toggle advanced options (key `t`).

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

Because the core of Elektra is minimal, plugins are needed to
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
- [spec](/src/plugins/spec) copies metadata from spec namespace
  to other namespaces.
  Needed for tests. (Required with `ENABLE_TESTING`, except on mingw.)
- [sync](/src/plugins/sync) is very useful to not lose any data.
  If you do not want to include it, make sure to set
  `/sw/elektra/kdb/#0/current/plugins` to a value not containing sync
  (e.g. an empty value).
  See [kdb-mount(1)](/doc/help/kdb-mount.md).

By default CMake adds nearly all plugins if the dependencies are present.
Only experimental plugins will be omitted by default:

```sh
-DPLUGINS="ALL;-EXPERIMENTAL"
```

To add also experimental plugins, you can use:

```sh
-DPLUGINS=ALL
```

> Note that plugins are only built if their dependencies are satisfied.
> So make sure to install all dependencies you need before you run `cmake`.
> For example, to include the plugin `yajl`, make sure `libyajl-dev` is installed.

To add all plugins except some plugins you can use:

```sh
-DPLUGINS="ALL;-plugin1;-plugin2"
```

For example, if you want all plugins except the jni plugin you would use:

```sh
-DPLUGINS="ALL;-jni"
```

To add all plugins not having additional dependencies
(they need only POSIX), you can use

```sh
-DPLUGINS=NODEP
```

Note, that every `infos/provides` and `infos/status` field written uppercase can
be used to select plugins that way (see README of [individual plugins](/src/plugins)).
You also can combine any of these fields
and add/remove other plugins to/from it, e.g. to include all plugins without deps,
that provide storage (except `yajl`) and are maintained, but not include all plugins
that are experimental, you would use:

```sh
-DPLUGINS="NODEP;STORAGE;-yajl;MAINTAINED;-EXPERIMENTAL"
```

The inclusion is determined by following preferences:

1. if the plugin is explicit excluded with `-plugin`
2. if the plugin is explicit included with `plugin`
3. if the plugin is excluded via a category `-CATEGORY`
4. if the plugin is included via a category `CATEGORY`
5. plugins are excluded if they are not mentioned at all
   (neither by category nor by name)

Note, that changing `PLUGINS` will not modify the defaults used
after Elektra was installed. For this endeavour you need to change:

```sh
-DKDB_DEFAULT_RESOLVER=resolver
```

and

```sh
-DKDB_DEFAULT_STORAGE=dump
```

The default resolver and storage will write to `KDB_DB_FILE` and `KDB_DB_INIT`
([for bootstrapping](/doc/help/elektra-bootstrapping.md)).

Obviously, you can pass the exact list of plugins you want, e.g.:

```sh
-DPLUGINS="resolver;sync;dump"
```

Some plugins are compile-time configurable. Then you can choose which
features are compiled in or out. This is especially important in the
bootstrapping phase, because then only the compiled in configuration
applies. To compile-time-configure a plugin, you just pass a underscore
(`_`) and flags after the name of the plugin.

The resolver for example distinguish between 3 different kind of flags:

```sh
-DPLUGINS="resolver_baseflags_userflags_systemflags"
```

The following base flags are available:

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

```sh
-DPLUGINS="resolver_lm_uhpb_b"
```

To add `resolver_l_h_b` you need to specify

```sh
-DPLUGINS="resolver;resolver_l_h_b"
```

You can add resolver with any combination of the flags, even if they are
not available in `ALL`.

#### Tools

Tools are used to add extra functionality to Elektra.
The flag used to specify which tools are compiled is
`-DTOOLS`, thus flag works similarly to the `-DPLUGINS` flag,
but is more limited in its functionality (which does not
matter, because there are not so many tools).

To add all non-experimental tools, you can use::

```sh
-DTOOLS=ALL
```

> Note that the behavior is different to PLUGINS
> which includes all PLUGINS if ALL is used.

To add all tools except of race, you can use:

```sh
-DTOOLS="ALL;-race"
```

To specify specific tools you can use, e.g.:

```sh
-DTOOLS=qt-gui;kdb
```

#### Bindings

Bindings are used in a like as `PLUGINS`.
For example, to build all maintained bindings and exclude experimental bindings
you can use:

```sh
-DBINDINGS=MAINTAINED;-EXPERIMENTAL
```

The SWIG executable may be specified with:

```sh
-DSWIG_EXECUTABLE=/usr/bin/swig3.0
```

If this option is not used, cmake will find the first occurrence of
`swig` in your environment's path.

Some bindings provide different APIs (and not a different language), e.g:

- `gsettings`
- `INTERCEPT` with `intercept_fs` and `intercept_env`
- `IO` with `io_uv`

To not add such APIs, but only `swig` bindings and `cpp`, you can use:

```sh
-DBINDINGS="SWIG;cpp"
```

For a list of available bindings see
[binding's README.md](/src/bindings/README.md).

#### `CMAKE_BUILD_TYPE`

`Debug`, `Release` or `RelWithDebInfo`
See help bar at bottom of `ccmake` for that option or:
http://www.cmake.org/Wiki/CMake_Useful_Variables

#### `BUILD_SHARED`, `BUILD_FULL` and `BUILD_STATIC`

`BUILD_SHARED` is the typical build you want to have on systems that support `dlopen`.
It can be used for desktop builds, but also embedded systems as long as they support
`dlopen`, for example, `BUILD_SHARED` is used on OpenWRT with `musl`.
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

```sh
cmake -DBUILD_SHARED=ON -DBUILD_FULL=ON -DBUILD_STATIC=OFF ..
```

#### `BUILD_DOCUMENTATION`

Build documentation with doxygen (API) and ronn-ng (man pages).

If ronn-ng is not found, already compiled man pages will be
used instead.

> Note: Turning off building the documentation, also turns off
> installing the documentation, see https://issues.libelektra.org/2522
> Then no man pages are available.

#### `BUILD_PDF`

Build documentation with LaTeX.

See [Documentation dependencies](#documentation-dependencies) for the required dependencies.

#### Developer Options

As developer you should enable `ENABLE_DEBUG` and `ENABLE_LOGGER`:

- `ENABLE_DEBUG`:
  - enables assertions
  - adds RTLD_NODELETE so that debugger finds symbols even after dlclose
- `ENABLE_LOGGER`:
  enables logging
  By default no logging will take place,
  see [CODING](/doc/CODING.md) for how to get log messages.

Continue reading [testing](/doc/TESTING.md) for more information about testing.

#### `CMAKE_INSTALL_PREFIX`

`CMAKE_INSTALL_PREFIX` defaults to `/usr/local`.
So by default most files will be installed below `/usr/local`.
Exceptions to this are files handled by [INSTALL_SYSTEM_FILES](#install_system_files).

Edit that cache entry to change that behavior.
Also called system prefix within the documentation.

If you want to create a package afterwards it is ok to use
paths that you can write to (e.g. `-DCMAKE_INSTALL_PREFIX=/home/username/`)

#### `LIB_SUFFIX`

Lets you install libraries into architecture specific folder.
E.g. for 32/64 bit systems you might install libraries under
`lib64`. Set `LIB_SUFFIX` to `64` to achieve exactly that.
So the system library folder will be `CMAKE_INSTALL_PREFIX/lib64`
then.

#### `TARGET_INCLUDE_FOLDER`

By default include folders will be installed below
`CMAKE_INSTALL_PREFIX/include/elektra`.
This entry let you change the elektra.
If the entry is empty, the include files will be
installed directly to `CMAKE_INSTALL_PREFIX/include`.

#### `TARGET_PLUGIN_FOLDER`

Similar to above, but with the plugins. Default is:
`CMAKE_INSTALL_PREFIX/lib${LIB_SUFFIX}/elektra`
It can be also left empty to install plugins next
to other libraries.

#### `GTEST_ROOT`

This value specifies the root directory of a local copy of the [Google Test][] framework.

- If it is empty (`""`), then the build system will download a copy of
  [Google Test][] into the build directory.
- Otherwise the build system will search for the file `CMakeLists.txt` in the
  top level directory of `GTEST_ROOT`. If this file exists, then the build system
  will use the sources files at `GTEST_ROOT` to translate tests that use [Google Test][].

It can be provided as CMake or environment variable.
If both options are provided the value passed via CMake takes precedence.

[google test]: https://github.com/google/googletest

It is recommended that you browse through all the options using `ccmake`.
Afterwards press `c` again (maybe multiple times until all variables are
resolved) and then `g` to generate. Finally press `e` to exit.

#### `INSTALL_BUILD_TOOLS`

Specifies that the build tools, i.e. `elektra-export-symbols` and `elektra-export-symbols`
are installed (by default off). Is needed for cross-compilation.

#### `INSTALL_SYSTEM_FILES`

Some of Elektra’s targets require to be installed into specific folders in the
file system hierarchy to work properly.

This variable is disabled by default, since it requires the install target to have the
rights to write into the corresponding folders. Set `-DINSTALL_SYSTEM_FILES=ON`,
if you also want to install the files listed below.

If you do not have root rights you can also copy the files manually to your user folder.

Currently the installed system files are as following:

| Module          | Description                     | Install Path                           |
| --------------- | ------------------------------- | -------------------------------------- |
| bash-completion | bash tab auto completion file   | `completionsdir` from pkg-config (¹)   |
| zsh-completion  | zsh tab auto completion file    | /usr/share/zsh/vendor-completions      |
| GIR             | introspection file for bindings | `INTROSPECTION_GIRDIR` from pkg-config |
| GSettings       | GSettings backend module        | `GIO_MODULE_DIR` from pkg-config       |

> (¹) Or `/usr/share/bash-completion/completions` as fallback.

#### `ENABLE_OPTIMIZATIONS`

In order to keep the binaries as small as possible this flag allows trading memory for speed.

## Building

### Without IDE

To build the source use:

```sh
make
```

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

```sh
mkdir build
cd build
cmake .. -G 'CodeBlocks - Unix Makefiles'
make package
```

**Note 1:**
You can use other editor if you like just type cmake at the
console to get a list of option you can pass to cmake as long as well
as a list of what code editor project cmake can create.

**Note 2:**
For Unix if you have nCurses install you can run `ccmake` to set important option after
running cmake like to enable debug symbol.

**Note 3:**
For Gentoo it's recommended to emerge sys-apps/lsb-release to name the package
right even thou not required.

## Maintainer's Guide

### Multiarch

On multiarch (multiple architectures installed in one system), you need to set `LIB_SUFFIX`.
For example, if you want to have the binaries in `lib64` and `lib32`, you
would use for the libraries to be installed in `${CMAKE_INSTALL_PREFIX}/lib64`:

```sh
-DLIB_SUFFIX="64"
```

If there is a directory for different architectures, simply prepend an `/`.
For example, for Debian:

```sh
-DLIB_SUFFIX="/$(DEB_HOST_MULTIARCH)"
```

### `RPATH`

By default Elektra uses `RPATH` to hide its plugins. This makes it obvious that
external applications should _not_ link against plugins. Instead every application
should use the `elektraModulesLoad()` API to load Elektra’s modules.

The folder where the plugins are located is a subdirectory of where the
libraries are installed. The name of the subdirectory can be specified
using `TARGET_PLUGIN_FOLDER` and is `elektra` by default. You might
want to encode Elektra’s `SOVERSION` into the folders name, if you want
different major versions of Elektra be co-installable.

Elektra’s use case for `RPATH` is considered acceptable, so we recommend using it
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

If you want Elektra to _not_ use `RPATH`, you can add:

```sh
-DTARGET_PLUGIN_FOLDER="" -DCMAKE_SKIP_INSTALL_RPATH=ON
```

Then all plugins are directly installed to the library directory and loaded
like other libraries (in any of `ld.so` paths).

Alternatively, which gives you the advantage not to clutter the main library path,
is to add the plugin folder in `/etc/ld.so.conf.d/elektra`. Note that it still allows
applications to link against plugins.

## Troubleshooting

### Dependencies not Available for Cent OS

Please enable EPEL https://fedoraproject.org/wiki/EPEL

```sh
# Install EPEL for RHEL 7
curl -o epel-release-7-8.noarch.rpm \
  http://dl.fedoraproject.org/pub/epel/7/x86_64/e/epel-release-7-8.noarch.rpm
sudo rpm -ivh epel-release-7-8.noarch.rpm
sudo yum update
```

For Bindings swig3 is recommended. swig2 only works on some distributions.
E.g., for Debian Jessie the bindings will crash.

At time of writing, no swig 3 was available, not even in EPEL.
Thus you need to install swig3 manually:

```sh
curl https://codeload.github.com/swig/swig/tar.gz/rel-3.0.10 | tar xz
cd swig-rel-3.0.10 && ./autogen.sh && ./configure && make
sudo make install
cd ..
```

Also, no ronn-ng was available, thus you need to do:

```sh
gem install ronn-ng -v 0.10.1.pre1
```

### Cross Compiling

In Elektra cross compiling needs two steps. If you get errors like
`elektra-export-errors_EXE_LOC` not found, go on reading.

In the first step, you need to compile Elektra for the host architecture
and install the build tools:

```sh
cmake -DINSTALL_BUILD_TOOLS=ON \
      -DCMAKE_PREFIX_PATH=$(STAGING_DIR_HOST) \
      ..
cmake --build . -- -j5
cmake --build . --target install
```

Where `$(STAGING_DIR_HOST)` must be a directory to be found in the later
build process. In particular, `$(STAGING_DIR_HOST)/bin` must be in a
directory found by a later `find_program`.

Then you need to compile Elektra again, but for the target architecture.
Now, the build tools such as `elektra-export-errors` should be found in
the `$(STAGING_DIR_HOST)/bin` where they were installed before.

For reference, you can look into the
[OpenWRT Elektra Makefile](https://github.com/openwrt/packages/blob/master/libs/elektra/Makefile)
and the
[CMake in OpenWRT](https://github.com/openwrt/openwrt/blob/master/include/cmake.mk).

## See Also

- [INSTALL](INSTALL.md)
- [TESTING](TESTING.md)
- [BUILDSERVER](BUILDSERVER.md)
