#
# CACHE
#
# Here the cache variables are set
if (PLUGINS MATCHES "ALL")
	set (PLUGINS
		ccode  dbus  doc  dump  error  fstab
		glob  hexcode  hidden  hosts  iconv  network  ni  null
		path  resolver  simpleini  struct  success  syslog  tcl
		template  timeofday  tracer  type  validation  xmltool
		yajl
		CACHE STRING "Which plugins should be compiled?"
		FORCE
		)
else ()
	set (PLUGINS
		dump resolver
		CACHE STRING "Which plugins should be compiled? ALL for all available"
		)
endif ()

#
# Runtime pathes for KDB
#

# May be changed to /etc/config when XDG will be implemented
set (KDB_DB_SYSTEM "/etc/kdb" CACHE PATH
		"The path to the system key database."
		)

set (KDB_DB_HOME "/home" CACHE PATH
		"The path to users home directories."
		)

# May be changed to .config when XDG will be implemented
set (KDB_DB_USER ".kdb" CACHE PATH
		"This path will be appended after the resolved home directory. It completes the path to the user key database."
		)



#
# Compile options
#


option (COMPILE_CXX11_MODE "Use the new C++11 standard" ON)





#
# Build properties
#


option (BUILD_SHARED "Build the shared version of elektra." ON)
option (BUILD_FULL "Build the full version of elektra (shared with all selected backends included)." ON)
option (BUILD_STATIC "Build the static version of elektra (all selected backends included statically)." ON)

option (BUILD_EXAMPLES "Build example applications using elektra." ON)

option (BUILD_DOCUMENTATION "Build the documentation (API, man pages)" ON)
if (BUILD_DOCUMENTATION)
	option (INSTALL_DOCUMENTATION "Install the documentation (API, man pages)" ON)
else (BUILD_DOCUMENTATION)
	#install documentation makes no sense if it is not build
	#(even though the option would not harm)
	set (INSTALL_DOCUMENTATION OFF CACHE BOOL
			"Install the documentation (API, man pages)"
			FORCE
		)
endif (BUILD_DOCUMENTATION)

option (ENABLE_TESTING "Enable to run tests by make test target" ON)
option (BUILD_TESTING "Build testcases" ON)
if (BUILD_TESTING)
	option (INSTALL_TESTING "Install testcases" ON)
elseif (BUILD_TESTING)
	#install testing makes no sense if it is not build
	#(even though the option would not harm)
	set (INSTALL_TESTING OFF CACHE BOOL "Install testcases" FORCE)
endif (BUILD_TESTING)

#
# Developer builds (debug or verbose build)
#

option (ELEKTRA_DEBUG_BUILD "Build with extra debug print messages (to debug elektra).")
if (ELEKTRA_DEBUG_BUILD)
	set (DEBUG "1")
else (ELEKTRA_DEBUG_BUILD)
	set (DEBUG "0")
endif (ELEKTRA_DEBUG_BUILD)
MARK_AS_ADVANCED(ELEKTRA_DEBUG_BUILD)

option (ELEKTRA_VERBOSE_BUILD "Build with even more print messages (to debug elektra).")
if (ELEKTRA_VERBOSE_BUILD)
	set (VERBOSE "1")
else (ELEKTRA_VERBOSE_BUILD)
	set (VERBOSE "0")
endif (ELEKTRA_VERBOSE_BUILD)
MARK_AS_ADVANCED(ELEKTRA_VERBOSE_BUILD)


#
# Target installation folders
#

set (TARGET_INCLUDE_FOLDER
		"elektra"
		CACHE STRING
		"Optional folder below system include folder to install include files."
    )

set (TARGET_CMAKE_FOLDER
		"share/cmake-${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}/Modules"
		CACHE STRING
		"The folder below system prefix where to install cmake files."
    )

set (TARGET_PLUGIN_FOLDER "elektra"
		CACHE STRING
		"Optional folder below system library folder where to install elektra plugins. LIB_SUFFIX is honored."
    )

set (TARGET_PKGCONFIG_FOLDER
		"pkgconfig"
		CACHE STRING
		"The folder below system library folder where to install pkgconfig files. LIB_SUFFIX is honored."
    )

set (TARGET_DOCUMENTATION_FOLDER
		"share/doc/elektra-api"
		CACHE STRING
		"The folder below system prefix where to install api documentation files."
    )


#
# Misc.
#

set (LIB_SUFFIX ""
		CACHE STRING
		"Optional suffix to use on lib folders (e.g. 64 for lib64)"
    )

set (MEMORYCHECK_COMMAND
		/usr/bin/valgrind
		CACHE FILEPATH
		"Full path to valgrind the memory checker"
    )

