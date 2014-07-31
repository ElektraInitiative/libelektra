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

For the plugins, please refer to the contract.h of the respective
plugin.



## PREPARATION ##

Elektra is using cmake.
Tested are cmake version 2.6-patch 0 and version 2.8.2.

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


### COMPILERS ###

For supported compilers see the automatic build farm on
http://build.libelektra.org:8080/

Additional gcc 4.6 armhf is tested regularly.



|   Compiler        |         Version             |      Target       |
|-------------------|-----------------------------|-------------------|
|      gcc          | gcc (Debian 4.7.2-5) 4.7.2  |      amd64        |
|      gcc          | (Debian 4.4.5-8) 4.4.5      |      amd64        |
|      gcc          | (Debian 4.4.5-8) 4.3        |      amd64        |
|      gcc          | 4.6                         |      armhf        |
|      gcc          | (Debian 4.4.5-8) 4.4.5      |      i386         |
|     clang         | version 1.1 (Debian 2.7-3)  |x86_64-pc-linux-gnu|


To change the compiler, use  

	CMAKE_C_COMPILER and CMAKE_CXX_COMPILER.

for example to use gcc-4.3  

	cmake -DCMAKE_C_COMPILER=gcc-4.3 -DCMAKE_CXX_COMPILER=g++-4.3 ..



### OPTIONS ###

#### PLUGINS ####
Because the core of elektra is minimal, plugins are needed to
actually read and write to configuration files (storage plugins),
commit the changes (resolver plugin) and also do many other
tasks related to configuration.
To add all plugins, you can use  

		-DPLUGINS=ALL
To add all plugins not having additional dependencies
(they need only POSIX), you can use  
	
		-DPLUGINS=NODEP
To manually set the default (same as not setting PLUGINS), you can use  

		-DPLUGINS=DEFAULT

#### CMAKE_BUILD_TYPE  ####
Debug, Release or RelWithDebInfo
See help bar at bottom of ccmake for that option or:
http://www.cmake.org/Wiki/CMake_Useful_Variables

#### DEBUG_BUILD and ELEKTRA_VERBOSE_BUILD  ####
Only needed by elektra developers.
Make the library to output some or a lot of things.
It is not recommended to use these options.

#### KDB_DB_SYSTEM ####
is the path where the system configuration is searched at runtime

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
 -j for parallel builds (use nr of CPUs+1)
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

