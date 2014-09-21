#
# CACHE
#
# Here all cache variables are set
#
#
# If you add something here, make sure to also add it in
# src/include/kdbversion.h.in


#
# the default list of plugins
#
# They are essential so that elektra can work
#
set (PLUGINS_LIST_DEFAULT
	dump
	resolver
	sync
	)

#
# force default list
# (default plugins are always included, so PLUGINS_LIST is unset)
#
if (PLUGINS MATCHES "DEFAULT")
	set (PLUGINS_FORCE FORCE)
endif ()

#
# Those plugins can only be compiled, but cannot be used.
# Should compile on every system where elektra compiles.
#
set (PLUGINS_LIST_COMPILE
	template
	doc
	)

#
# Plugins which only need Ansi C/C++,
# (like elektra core itself)
# Should compile on every system where elektra compiles.
#
set (PLUGINS_LIST_NODEP
	ccode
	error
	fstab
	hexcode
	hidden
	ni
	null
	struct
	tracer
	type
	validation
	constants
	noresolver
	ini
	)

#
# Plugins which use some posix facility
#
set (PLUGINS_LIST_POSIX
	glob
	hosts
	iconv
	network
	path
	keytometa
	syslog
	uname
	timeofday
	simpleini
	line
	resolver_c_b_b  # needed for tests
	)

#
# force no dependency list
#
if (PLUGINS MATCHES "NODEP")
	set (PLUGINS_LIST
		${PLUGINS_LIST_COMPILE}
		${PLUGINS_LIST_NODEP}
		${PLUGINS_LIST_POSIX}
	    )
	set (PLUGINS_FORCE FORCE)
endif ()


#
# plugins with dependencies
#
set (PLUGINS_LIST_DEP
	yajl
	dbus
	tcl
	xmltool
	augeas
	journald
	)

#
# force all plugins
#
if (PLUGINS MATCHES "ALL")
	set (PLUGINS_LIST
		${PLUGINS_LIST_COMPILE}
		${PLUGINS_LIST_NODEP}
		${PLUGINS_LIST_POSIX}
		${PLUGINS_LIST_DEP}
		)
	set (PLUGINS_FORCE FORCE)
endif ()




#
# now actually set the plugins cache variable
#
# Always include DEFAULT plugins, but maybe include more
#
set (PLUGINS
	${PLUGINS_LIST_DEFAULT}
	${PLUGINS_LIST}
	CACHE STRING "Which plugins should be compiled? ALL for all available, NODEP for plugins without additional dependencies, DEFAULT for minimal set."
	${PLUGINS_FORCE}
	)




#
# set TOOLS cache variable
#
set (TOOLS_LIST_DEFAULT kdb)

if (TOOLS MATCHES "DEFAULT")
	set (TOOLS_FORCE FORCE)
endif ()

if (TOOLS MATCHES "NODEP")
	set (TOOLS_LIST
		)
	set (TOOLS_FORCE FORCE)
endif ()

if (TOOLS MATCHES "ALL")
	set (TOOLS_LIST
		gen
		)
	set (TOOLS_FORCE FORCE)
endif ()

set (TOOLS
	${TOOLS_LIST_DEFAULT}
	${TOOLS_LIST}
	CACHE STRING "Which TOOLS should be compiled? ALL for all available, NODEP for TOOLS without additional dependencies, DEFAULT for minimal set."
	${TOOLS_FORCE}
	)


#
# Runtime pathes for KDB
#

# May be changed to /etc/config when XDG will be implemented
set (KDB_DB_SYSTEM "/etc/kdb" CACHE PATH
		"The path to the system key database."
		)

set (KDB_DB_HOME "/home" CACHE PATH
		"The compiled-in fallback path to users home directories."
		)

# May be changed to .config when XDG will be implemented
set (KDB_DB_USER ".kdb" CACHE PATH
		"This path will be appended after the resolved home directory. It completes the path to the user key database."
		)



#
# Compile options
#


option (ENABLE_CXX11 "Include code using C++11 standard, needs gcc 4.7 or comparable clang/icc" OFF)

set (GTEST_ROOT "" CACHE PATH "use external gtest instead of internal")

set (CMAKE_PIC_FLAGS "-fPIC"
	CACHE STRING "Which pic flags should be used for cases cmake cannot handle it itself")

set (CMAKE_STATIC_FLAGS ""
	CACHE STRING "Which static flags should be used for compilation of *-static libs+tools, use \"-static\" if you want a real static kdb-static (it needs .a for every dependency though)")




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
	option (BUILD_PDF "Build the documentation also in PDF form" OFF)
else (BUILD_DOCUMENTATION)
	#install documentation makes no sense if it is not build
	#(even though the option would not harm)
	set (INSTALL_DOCUMENTATION OFF CACHE BOOL
			"Install the documentation (API, man pages)"
			FORCE
		)
	set (BUILD_PDF OFF CACHE BOOL
			"Build the documentation also in PDF form"
			FORCE
		)
endif (BUILD_DOCUMENTATION)

option (ENABLE_TESTING "Enable to run (any) tests by make test" ON)
if (ENABLE_TESTING)
	option (ENABLE_KDB_TESTING "Enable to run tests writing to hard disc" ON)
else (ENABLE_TESTING)
	set (ENABLE_KDB_TESTING OFF CACHE BOOL "Enable to run tests writing to hard disc" FORCE)
endif (ENABLE_TESTING)



option (BUILD_TESTING "Build main test suite (does not affect plugins+bindings)" ON)
if (BUILD_TESTING)
	option (INSTALL_TESTING "Install testcases" ON)
else (BUILD_TESTING)
	#install testing makes no sense if it is not build
	#(even though the option would not harm)
	set (INSTALL_TESTING OFF CACHE BOOL "Install testcases" FORCE)
endif (BUILD_TESTING)

set (ENABLE_COVERAGE OFF CACHE BOOL "enable coverage analysis (using gcov)")
set (COVERAGE_PREFIX
		"${PROJECT_SOURCE_DIR}/.."
		CACHE FILEPATH
		"Full path to common prefix of build+source directory"
    )


option (BUILD_SWIG_PYTHON2 "Enable the SWIG bindings for Python2" OFF)
option (BUILD_SWIG_PYTHON3 "Enable the SWIG bindings for Python3" OFF)
option (BUILD_SWIG_LUA    "Enable the SWIG bindings for Lua" OFF)
if (BUILD_SWIG_LUA)
	set (TARGET_LUA_CMOD_FOLDER "lib${LIB_SUFFIX}/lua/5.2"
		CACHE PATH
		"Directory to install Lua binary modules (configure lua via LUA_CPATH)"
	)
endif (BUILD_SWIG_LUA)

option (BUILD_GLIB "Enable GLIB bindings" OFF)
option (BUILD_GLIB_GI "Enable the GObject Introspection bindings" OFF)
if (BUILD_GLIB_GI AND NOT BUILD_GLIB)
	message (WARNING "GObject Introspection bindings require GLib bindings")
endif ()
if (BUILD_GLIB_GI)
	set (TARGET_LUA_LMOD_FOLDER "share/lua/5.2"
		CACHE PATH
		"Directory to install Lua modules (configure lua via LUA_PATH)"
	)
endif (BUILD_GLIB_GI)

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
		"This folder (below prefix/include) will be used to install include files."
    )

set (TARGET_CMAKE_FOLDER
		"share/cmake-${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}/Modules"
		CACHE STRING
		"This folder (below prefix) will be used to install cmake files."
    )

set (TARGET_PLUGIN_FOLDER
		"elektra"
		CACHE STRING
		"This folder (below prefix/lib) will be used to install elektra plugins. LIB_SUFFIX is honored."
    )

set (TARGET_PKGCONFIG_FOLDER
		"pkgconfig"
		CACHE STRING
		"The folder (below prefix/lib) folder where to install pkgconfig files. LIB_SUFFIX is honored."
    )

set (TARGET_DOCUMENTATION_HTML_FOLDER
		"share/doc/elektra-api/html"
		CACHE STRING
		"The folder (below prefix) where to install html api documentation files."
    )

set (TARGET_DOCUMENTATION_MAN_FOLDER
		"share/man/man3"
		CACHE STRING
		"The folder (below prefix) where to install man3 api documentation files."
    )

set (TARGET_DOCUMENTATION_LATEX_FOLDER
		"share/doc/elektra-api/latex"
		CACHE STRING
		"The folder (below prefix) where to install latex api documentation files."
    )

set (TARGET_TOOL_EXEC_FOLDER
		"lib${LIB_SUFFIX}/elektra/tool_exec"
		CACHE STRING
		"This folder (below prefix) will be used to install additional kdb-tools"
    )

set (TARGET_TEST_DATA_FOLDER
		"share/elektra/test_data"
		CACHE STRING
		"This folder (below prefix) will be used to install test data"
    )

set (TARGET_TEMPLATE_FOLDER
		"share/elektra/templates"
		CACHE STRING
		"This folder (below prefix) will be used to install templates"
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

set(DISCLAMER "
/***************************************************************************
 *                                                                         *
 * This header file is AUTO GENERATED.                                     *
 * Do not edit this file, otherwise your changes will be discarded in the  *
 * next cmake run.                                                         *
 *                                                                         *
 ***************************************************************************/")


MARK_AS_ADVANCED(FORCE
	# might be relevant to users:
	GTEST_ROOT
	COVERAGE_PREFIX
	Boost_DIR

	# are kind of internal:
	SWIG_DIR SWIG_EXECUTABLE SWIG_VERSION
	gtest_build_samples gtest_build_tests gtest_disable_pthreads
	gtest_force_shared_crt BUILD_SHARED_LIBS
	)
