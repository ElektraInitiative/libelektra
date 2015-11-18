# COMPILE #

## DEPENDENCIES ##

For the base system you only need cmake and build-essential (make, gcc,
some unix tools).

To build documentation you need doxygen and graphviz.

To build pdf documentation you need pdflatex with

	texlive-fonts-recommended
	texlive-latex-recommended

For the debian package, please refer to debian/control (in the debian
branch).

For the plugins, please refer to the README.md of the respective
plugin.



## PREPARATION ##

Elektra is using cmake.
Tested are cmake version 2.8.9 and version 3.0.2.

To configure Elektra graphically (with curses) run (.. belongs to command):

	mkdir build && cd build && ccmake ..

and press 'c':


All options described here, can also be used with cmake rather then
ccmake (.. belongs to the command!):

	mkdir build && cd build && cmake -D<OPTION1>=<VAR1> -D<OPTION2>=<VAR2> ..

For information what you can use as OPTION1 and OPTION2 see above.
Note to pass "" to VAR if it contains ;.
E.g.:

	cmake -DPLUGINS="dump;resolver;yajl" ..

Some scripts in the folder of the same name may help you running cmake.


### COMPILERS ###

For supported compilers see the automatic build farm on
http://build.libelektra.org:8080/

Additional gcc 4.6 armhf is tested regularly.



|   Compiler        |         Version             |      Target       |
|-------------------|-----------------------------|-------------------|
|      gcc          | gcc (Debian 4.7.2-5) 4.7.2  |      i386         |
|      gcc          | gcc (Debian 4.7.2-5) 4.7.2  |      amd64        |
|      gcc          | gcc 4.8                     |      amd64        |
|      gcc          | gcc 4.9                     |      amd64        |
|      gcc          | (Debian 4.4.5-8) 4.4.5      |      amd64        |
|      gcc          | (Debian 4.4.5-8) 4.3        |      amd64        |
|      gcc          | 4.6                         |      armhf        |
|      mingw        | 4.6                         |      i386         |
|      clang        | version 3.5.0-1~exp1        |x86_64-pc-linux-gnu|
|      icc          | 14.0.2 20140120             |x86_64-pc-linux-gnu|


To change the compiler, use  

	CMAKE_C_COMPILER and CMAKE_CXX_COMPILER.

for example to use gcc-4.3  

	cmake -DCMAKE_C_COMPILER=gcc-4.3 -DCMAKE_CXX_COMPILER=g++-4.3 ..



### OPTIONS ###

Some options, i.e. PLUGINS, BINDINGS and TOOLS are either:

- a list of elements separated with ;
   (note that shells typically need ; to be escaped)
- a special uppercase element that gets replaced by a list of elements, that are:
  - ALL to include all elements (except elements with unfulfilled dependencies)
  - NODEP to include all elements without dependencies
  - DEFAULT to go back to the situation that was there when nothing was changed
- elements prefixed with a minus symbol (-) to exclude an element

Examples for this are especially in the subsection PLUGINS below, but they work in the
same fashion for BINDINGS and TOOLS.

#### PLUGINS ####

Because the core of elektra is minimal, plugins are needed to
actually read and write to configuration files (storage plugins),
commit the changes (resolver plugin, also takes care about how
the configuration files are named) and also do many other
tasks related to configuration.

To add all plugins, you can use:

	-DPLUGINS=ALL

Note that plugins get dropped when dependencies are not satisfied.
To add all plugins except some plugins you can use:

	-DPLUGINS="ALL;-plugin1;-plugin2"

E.g. if you want all plugins except the jni plugin you would use:

	-DPLUGINS="ALL;-jni"

To add all plugins not having additional dependencies
(they need only POSIX), you can use  

	-DPLUGINS=NODEP

To manually set the default (same as not setting PLUGINS), you can use  

	-DPLUGINS=DEFAULT

This only states the list of the plugins are the default list and does
not mean that a different default is used after Elektra was installed.
For this endeavour you need to change:

	-DKDB_DEFAULT_RESOLVER=resolver

and

	-DKDB_DEFAULT_STORAGE=dump

Alternatively, you can pass the list of plugins you want, e.g.:

	-DPLUGINS="resolver;sync;dump"

Some plugins are compile-time configureable. Then you can choose which
features are compiled in or out. This is especially important in the
bootstrapping phase, because then only the compiled in configuration
applies. To compile-time-configure a plugin, you just pass a underscore
(_) and flags after the name of the plugin.

Note that the base-plugin itself need to be part of PLUGINS, so that the
variants will work.

The resolver even distinguish between 3 different kind of flags:

	-DPLUGINS="resolver_baseflags_userflags_systemflags"

Following baseflags are available:

- 'c' for debugging conflicts
- 'l' for enabling file locking
- 'm' for enabling mutex locking


The user flags are (the order matters!):

- 'p' use passwd/ldap to lookup home directory using getpwuid_r
- 'h' use the environment variable HOME
- 'u' use the environment variable USER
- 'b' use the built-in default CMAKE variable KDB_DB_HOME

The system flags are (the order matters!):

- if a path that begins with / is chosen the system flags are irrelevant
  and the path is taken as-is.
- 'x' use the environment variable XDG_CONFIG_DIRS
  (: are interpreted as part of filename, no searching is done!)
  This option is not recommended (unless for testing), because it
  allows users to fake system configuration.
- 'b' use the built-in default CMAKE variable KDB_DB_SYSTEM


E.g. one may use:

	-DPLUGINS="resolver;resolver_lm_uhpb_b"

#### TOOLS ####

Tools are used to add extra functionality to Elektra.
The flag used to specify which tools are compiled is
`-DTOOLS`, thus flag works similarly to the `-DPLUGINS` flag.

To add all tools, you can use::

	-DTOOLS=ALL

To add all plugins not having additional dependencies
(they need only POSIX), you can use:

	-DTOOLS=NODEP

To build only the default tools, you can use:

	-DTOOLS=DEFAULT

To specify specific tools you can use, e.g.:

	-DTOOLS=qt-gui;kdb


#### BINDINGS ####

Bindings are used in the same way as PLUGINS and TOOLS.
For example you can use:

	-DBINDINGS=ALL

Note that the same languages are sometimes available over GI and SWIG.
In this case, the SWIG bindings are preferred.
To add all swig bindings (and also cpp), you can use:

	-DBINDINGS=SWIG;cpp

The SWIG executable my be specified with:

	-DSWIG_EXECUTABLE=...

If this option is not used, cmake will find the first occurrence of
``swig`` in your environment's path.

#### CMAKE_BUILD_TYPE  ####
Debug, Release or RelWithDebInfo
See help bar at bottom of ccmake for that option or:
http://www.cmake.org/Wiki/CMake_Useful_Variables

#### ELEKTRA_DEBUG_BUILD and ELEKTRA_VERBOSE_BUILD  ####

Only needed by Elektra developers.
Make the library to output logging information.
It is not recommended to use these options.

#### BUILD_DOCUMENTATION ####
build documentation with doxygen the kdb tool does only have the integrated docu at the moment


#### CMAKE_INSTALL_PREFIX ####
By default all files will installed below /usr/local.
Edit that cache entry to change that behaviour.
Also called system prefix within the documentation.

If you want to create a package afterwards it is ok to use
pathes that you can write to (e.g. CMAKE_INSTALL_PREFIX /home/markus/bin)

#### LIB_SUFFIX ####
Lets you install libraries into architecture specific folder.
E.g. for 32/64 bit systems you might install libraries under
lib64. Set LIB_SUFFIX to 64 to achieve exactly that.
So the system library folder will be CMAKE_INSTALL_PREFIX/lib64
then.

#### TARGET_INCLUDE_FOLDER ####
By default include folders will be installed below
CMAKE_INSTALL_PREFIX/include/elektra
This entry let you change the elektra.
If the entry is empty, the include files will be
installed directly to CMAKE_INSTALL_PREFIX/include.

#### TARGET_PLUGIN_FOLDER ####
Similar to above, but with the plugins. Default is:
CMAKE_INSTALL_PREFIX/lib${LIB_SUFFIX}/elektra
It can be also left empty to install plugins next
to other libraries.


#### GTEST_ROOT ####
Specifies the root of the GoogleTest sources, to be used
for some of the tests. A CMakeLists.txt inside GTEST_ROOT
will be searched as way to detect a valid GoogleTest source
directory.
If it is "", an internal version of gtest will be used.

It is recommended that you browse through all of the options.
Afterwards press c again (maybe multiple times until all variables are
resolved) and then g to generate.  Finally press e to exit.



## BUILDING ##

### NO IDE ###

To build the source use:

    make

You can pass:
 -j for parallel builds
 VERBOSE=1 to see the invocations of the compiler


### CodeBlocks ###

You can build Elektra using Code::Blocks under Gentoo:

Precondition:
Make sure you have a compiler, xml2 (for kdb tool) and xsl (see later)
installed. cmake configure will help you with that, it will make sure you don't forget something
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

