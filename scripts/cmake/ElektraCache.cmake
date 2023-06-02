# ~~~
# CACHE
#
# Here all cache variables are set
#
#
# If you add something here, make sure to also add it in
# src/plugins/constants/
# ~~~

include (LibAddMacros)

set (PLUGINS_DOC "Which plugins should be added? ALL\;-EXPERIMENTAL is default. See doc/COMPILE.md")
set (
	PLUGINS
	"ALL;-EXPERIMENTAL"
	CACHE STRING ${PLUGINS_DOC})

set (INFO_PLUGINS_DOC "only for informational purposes. Modify PLUGINS to change the list.")
set (ADDED_PLUGINS_DOC "List of plugins already added, ${INFO_PLUGINS_DOC}")
set (
	ADDED_PLUGINS
	""
	CACHE STRING ${PLUGINS_DOC} FORCE)

set (SHARED_ONLY_PLUGINS_DOC "List of plugins already added with the ONLY_SHARED configuration enabled, ${INFO_PLUGINS_DOC}")

set (REMOVED_PLUGINS_DOC "List of plugins removed, ${INFO_PLUGINS_DOC} ")
set (
	REMOVED_PLUGINS
	""
	CACHE STRING ${PLUGINS_DOC} FORCE)

set (ADDED_DIRECTORIES_DOC "List of directories already added, ${INFO_PLUGINS_DOC}")
set (
	ADDED_DIRECTORIES
	""
	CACHE STRING ${PLUGINS_DOC} FORCE)

#
# set BINDINGS cache variable
#

set (BINDINGS_DOC "Which bindings should be added? ALL for all available, DEFAULT for minimal set, see doc/COMPILE.md.")

# TODO include ruby when issue #1770 is resolved (also in scripts/configure-*)
set (
	BINDINGS
	"MAINTAINED;-EXPERIMENTAL;-DEPRECATED;-ruby"
	CACHE STRING ${BINDINGS_DOC})

set (INFO_BINDINGS_DOC "only for informational purposes. Modify BINDINGS to change the list.")
set (ADDED_BINDINGS_DOC "List of bindings already added, ${INFO_BINDINGS_DOC}")
set (
	ADDED_BINDINGS
	""
	CACHE STRING ${BINDINGS_DOC} FORCE)

#
# set LIBRARIES cache variable
#

set (ADDED_LIBRARIES_DOC "List of plugins already added, only for informational purposes. We always build all libraries.")
set (
	ADDED_LIBRARIES
	""
	CACHE STRING ${ADDED_LIBRARIES_DOC} FORCE)

#
# set TOOLS cache variable
#

remember_for_removal (TOOLS TO_REMOVE_TOOLS)

set (TOOLS_LIST_DEFAULT kdb)

if (TOOLS MATCHES "DEFAULT")
	set (TOOLS_FORCE FORCE)
	list (REMOVE_ITEM TOOLS DEFAULT)
endif ()

if (TOOLS MATCHES "NODEP")
	set (TOOLS_LIST race)
	set (TOOLS_FORCE FORCE)
	list (REMOVE_ITEM TOOLS NODEP)
endif ()

if (TOOLS MATCHES "ALL")
	set (
		TOOLS_LIST
		elektrad
		fuse
		gen-gpg-testkey
		hub-zeromq
		kdb
		pythongen
		qt-gui
		race
		webd
		website
		webui)
	set (TOOLS_FORCE FORCE)
	list (REMOVE_ITEM TOOLS ALL)
endif ()

set (
	TOOLS_DOC
	"Which TOOLS should be added? Either list individual tools or ALL for all available (including website!), NODEP for TOOLS without additional dependencies, DEFAULT is only kdb."
)

set (
	TOOLS
	${TOOLS_LIST_DEFAULT} ${TOOLS_LIST} ${TOOLS}
	CACHE STRING ${TOOLS_DOC} ${TOOLS_FORCE})

removal (TOOLS TO_REMOVE_TOOLS)
set (
	TOOLS
	${TOOLS}
	CACHE STRING ${TOOLS_DOC} FORCE)

#
# Runtime paths for KDB
#

set (
	KDB_DB_SYSTEM
	"/etc/kdb"
	CACHE STRING "The path to the system key database. Relative paths are append to CMAKE_INSTALL_PREFIX.")

set (
	KDB_DB_HOME
	"/home"
	CACHE STRING "The compiled-in fallback path to users home directories.")

set (
	KDB_DB_USER
	".config"
	CACHE STRING "This path will be appended after the resolved home directory. It completes the path to the user key database.")

set (
	KDB_DB_SPEC
	"share/elektra/specification"
	CACHE STRING "This path will be appended after the prefix. It completes the path to the specification key database.")

set (
	KDB_DB_DIR
	".dir"
	CACHE STRING "The configuration directory for config files in dir namespace.")

set (
	KDB_DB_FILE
	"default.ecf"
	CACHE STRING "This configuration file will be used as default if no root mount point available.")

set (
	KDB_DB_INIT
	"elektra.ecf"
	CACHE STRING "This configuration file will be used for bootstrapping.")

set (
	KDB_DEFAULT_STORAGE
	"dump"
	CACHE STRING "This storage plugin will be used initially (as default and for bootstrapping).")

if (KDB_DEFAULT_STORAGE STREQUAL "storage")
	message (FATAL_ERROR "KDB_DEFAULT_STORAGE must not be storage, pick a concrete storage, e.g. dump or ini")
endif ()

set (
	KDB_DEFAULT_RESOLVER
	"resolver_fm_hpu_b"
	CACHE STRING "This resolver plugin will be used initially (as default and for bootstrapping).")

if (KDB_DEFAULT_RESOLVER STREQUAL "resolver")
	message (FATAL_ERROR "KDB_DEFAULT_RESOLVER must not be resolver, pick one of the variants, e.g. resolver_fm_hpu_b or wresolver")
endif ()

#
# Compile options
#

set (
	GTEST_ROOT
	"$ENV{GTEST_ROOT}"
	CACHE PATH "This value specifies the path to a local version of Google Test. \
If you leave it blank, then the build system will download \
a copy of Google Test into the build directory.")

set (
	CMAKE_PIC_FLAGS
	"-fPIC"
	CACHE STRING "Which pic flags should be used for cases cmake cannot handle it itself")

set (
	CMAKE_STATIC_FLAGS
	""
	CACHE
		STRING
		"Which static flags should be used for compilation of *-static libs+tools, use \"-static\" if you want a real static kdb-static (it needs .a for every dependency though)"
)

#
# Build properties
#

option (BUILD_SHARED "Build the shared version of elektra." ON)
option (BUILD_FULL "Build the full version of elektra (shared with all selected backends included)." OFF)
option (BUILD_STATIC "Build the static version of elektra (all selected backends included statically)." OFF)

option (BUILD_DOCUMENTATION "Build the documentation (API, man pages)" ON)
if (BUILD_DOCUMENTATION)
	option (INSTALL_DOCUMENTATION "Install the documentation (API, man pages)" ON)
	option (BUILD_PDF "Build the documentation also in PDF form" OFF)
	option (BUILD_DOCSET "Generate a DocSet usable in applications such as Xcode, Dash and Zeal" OFF)
else (BUILD_DOCUMENTATION)

	# install documentation makes no sense if it is not build (even though the option would not harm)
	set (
		INSTALL_DOCUMENTATION
		OFF
		CACHE BOOL "Install the documentation (API, man pages)" FORCE)
	set (
		BUILD_PDF
		OFF
		CACHE BOOL "Build the documentation also in PDF form" FORCE)
endif (BUILD_DOCUMENTATION)

option (ENABLE_TESTING "Enable to run (any) tests by make test" ON)
if (ENABLE_TESTING)
	option (ENABLE_KDB_TESTING "Enable to run tests writing to hard disc" ON)
else (ENABLE_TESTING)
	set (
		ENABLE_KDB_TESTING
		OFF
		CACHE BOOL "Enable to run tests writing to hard disc" FORCE)
endif (ENABLE_TESTING)

option (BUILD_TESTING "Build main test suite (does not affect plugins+bindings)" ON)
if (BUILD_TESTING)
	option (INSTALL_TESTING "Install test cases" ON)
else (BUILD_TESTING)

	# install testing makes no sense if it is not build (even though the option would not harm)
	set (
		INSTALL_TESTING
		OFF
		CACHE BOOL "Install test cases" FORCE)
endif (BUILD_TESTING)

option (INSTALL_SYSTEM_FILES "Install files to system directories" OFF)

option (INSTALL_BUILD_TOOLS "Install build tools for cross-compilation" OFF)

option (ENABLE_OPTIMIZATIONS "Turn on optimizations that trade memory for speed" ON)

#
# Developer builds
#

option (ENABLE_ASAN "Activate sanitizers and force RTLD_NODELETE, see doc/TESTING.md.")
if (ENABLE_ASAN)
	set (ASAN "1")
else (ENABLE_ASAN)
	set (ASAN "0")
endif (ENABLE_ASAN)

set (
	ENABLE_COVERAGE
	OFF
	CACHE BOOL "Enable coverage analysis (using gcov), see doc/TESTING.md.")
set (
	COVERAGE_PREFIX
	"${PROJECT_SOURCE_DIR}"
	CACHE FILEPATH "Full path to common prefix of build+source directory")

option (ENABLE_DEBUG "Build with assertions and use RTLD_NODELETE. Intended for developing and debugging Elektra.")
if (ENABLE_DEBUG)
	set (DEBUG "1")
else (ENABLE_DEBUG)
	set (DEBUG "0")
endif (ENABLE_DEBUG)

option (ENABLE_LOGGER "Allows Elektra to write logs.")
if (ENABLE_LOGGER)
	set (HAVE_LOGGER "1")
else (ENABLE_LOGGER)
	set (HAVE_LOGGER "0")
endif (ENABLE_LOGGER)

#
# Target installation folders
#

set (
	TARGET_INCLUDE_FOLDER
	"elektra"
	CACHE STRING "This folder (below prefix/include) will be used to install include files.")

set (
	TARGET_CMAKE_FOLDER
	"lib${LIB_SUFFIX}/cmake/Elektra"
	CACHE STRING "This folder (below prefix) will be used to install cmake files.")

set (
	TARGET_PLUGIN_FOLDER
	"elektra"
	CACHE STRING "This folder (below prefix/lib) will be used to install elektra plugins. LIB_SUFFIX is honored.")

set (
	TARGET_PKGCONFIG_FOLDER
	"pkgconfig"
	CACHE STRING "The folder (below prefix/lib) folder where to install pkgconfig files. LIB_SUFFIX is honored.")

set (
	TARGET_DOCUMENTATION_TEXT_FOLDER
	"share/doc/elektra"
	CACHE STRING "The folder (below prefix) where to install textual documentation files.")

set (
	TARGET_DOCUMENTATION_HTML_FOLDER
	"share/doc/elektra-api/html"
	CACHE STRING "The folder (below prefix) where to install html api documentation files.")

set (
	TARGET_DOCUMENTATION_MAN_FOLDER
	"share/man/man3"
	CACHE STRING "The folder (below prefix) where to install man3 api documentation files.")

set (
	TARGET_DOCUMENTATION_LATEX_FOLDER
	"share/doc/elektra-api/latex"
	CACHE STRING "The folder (below prefix) where to install latex api documentation files.")

set (
	TARGET_DOCUMENTATION_DOC-BASE_FOLDER
	"share/doc-base/"
	CACHE STRING "The folder (below prefix) where to install doc-base documentation files for debian packaging.")

set (
	TARGET_TOOL_EXEC_FOLDER
	"lib${LIB_SUFFIX}/elektra/tool_exec"
	CACHE STRING "This folder (below prefix) will be used to install additional kdb-tools")

set (
	TARGET_TOOL_DATA_FOLDER
	"share/elektra/tool_data"
	CACHE STRING "The folder (below prefix) where to install tool data files.")

set (
	TARGET_TEST_DATA_FOLDER
	"share/elektra/test_data"
	CACHE STRING "This folder (below prefix) will be used to install test data")

set (
	TARGET_TEMPLATE_FOLDER
	"share/elektra/templates"
	CACHE STRING "This folder (below prefix) will be used to install templates")

set (
	TARGET_LUA_CMOD_FOLDER
	"lib${LIB_SUFFIX}/lua/5.2"
	CACHE STRING "Directory to install Lua binary modules, should be in LUA_CPATH")

set (
	TARGET_LUA_LMOD_FOLDER
	"share/lua/5.2"
	CACHE STRING "Directory to install Lua source modules, should be in LUA_PATH)")

if (NOT TARGET_PLUGIN_FOLDER STREQUAL "")
	if (CMAKE_SKIP_INSTALL_RPATH)
		message (
			WARNING
				"You specified to remove RPATH, but TARGET_PLUGIN_FOLDER is not an empty string. Please read doc/COMPILE.md#RPATH"
		)
	endif ()
endif ()

if (CMAKE_SKIP_BUILD_RPATH AND BUILD_TESTING)
	message (WARNING "You specified to remove RPATH, but did not disable tests. Please read doc/COMPILE.md#RPATH")
endif ()

#
# Misc.
#

set (
	LIB_SUFFIX
	""
	CACHE STRING "Optional suffix to use on lib folders (e.g. 64 for lib64)")

set (MEMORYCHECK_COMMAND_OPTIONS "--trace-children=yes --leak-check=full --gen-suppressions=all --error-exitcode=1")
set (
	MEMORYCHECK_SUPPRESSIONS_FILE
	"${CMAKE_SOURCE_DIR}/tests/valgrind.suppression"
	CACHE FILEPATH "Full path to suppression file for valgrind")

set (
	DISCLAMER
	"
/***************************************************************************
 *                                                                         *
 * This header file is AUTO GENERATED.                                     *
 * Do not edit this file, otherwise your changes will be discarded in the  *
 * next cmake run.                                                         *
 *                                                                         *
 ***************************************************************************/")

mark_as_advanced (
	FORCE # The following settings might be relevant to a few users:
	DISCOUNT_DIR
	DREDD_EXECUTABLE
	GO_EXECUTABLE
	Qt5QmlModels_DIR
	TARGET_LUA_CMOD_FOLDER
	TARGET_LUA_LMOD_FOLDER
	GTEST_ROOT
	BUILD_GMOCK
	BUILD_GTEST
	INSTALL_GMOCK
	INSTALL_GTEST
	LIBFA_INCLUDE_DIR
	gmock_build_tests
	gtest_hide_internal_symbols
	COVERAGE_PREFIX
	CMAKE_PIC_FLAGS
	CMAKE_STATIC_FLAGS
	SWIG_EXECUTABLE
	GRADLE_EXECUTABLE
	NPM_EXECUTABLE
	RONN_LOC
	GIT_EXECUTABLE
	LIBGCRYPT_INCLUDE_DIR
	XercesC_DIR
	OPENSSL_INCLUDE_DIR)
mark_as_advanced (
	LUA_EXECUTABLE # The following settings are internal (not to be changed by users):
	FEDORA
	CARGO_EXECUTABLE
	DIFF_COMMAND
	GLib_CONFIG_INCLUDE_DIR
	GLib_INCLUDE_DIRS
	pkgcfg_lib_GIO_gio-2.0
	pkgcfg_lib_GIO_glib-2.0
	pkgcfg_lib_GIO_gobject-2.0
	pkgcfg_lib_GLIB_glib-2.0
	pkgcfg_lib_GLib_PKG_glib-2.0
	pkgcfg_lib_GMODULE_glib-2.0
	pkgcfg_lib_GMODULE_gmodule-2.0
	pkgcfg_lib_GOBJECT_glib-2.0
	pkgcfg_lib_GOBJECT_gobject-2.0
	pkgcfg_lib_PC_LIBXML_xml2
	pkgcfg_lib_PC_ZeroMQ_zmq
	pkgcfg_lib_PC_libuv_dl
	pkgcfg_lib_PC_libuv_nsl
	pkgcfg_lib_PC_libuv_pthread
	pkgcfg_lib_PC_libuv_rt
	pkgcfg_lib_PC_libuv_uv
	pkgcfg_lib_PC_yaml-cpp_yaml-cp
	pkgcfg_lib__DBUS_PC_dbus-1
	pkgcfg_lib__LIBSYSTEMD_PC_syst
	pkgcfg_lib__OPENSSL_crypto
	pkgcfg_lib__OPENSSL_ssl
	pkgcfg_lib_PC_yaml-cpp_yaml-cpp
	pkgcfg_lib__LIBSYSTEMD_PC_systemd
	SWIG_DIR
	SWIG_VERSION
	gtest_build_samples
	gtest_build_tests
	gtest_disable_pthreads
	gtest_force_shared_crt
	BUILD_SHARED_LIBS
	ADDED_DIRECTORIES
	ADDED_PLUGINS
	REMOVED_PLUGINS
	REMOVED_TOOLS
	ADDED_BINDINGS
	ADDED_LIBRARIES
	LIBGCRYPTCONFIG_EXECUTABLE
	jna
	Qt5Core_DIR
	Qt5Gui_DIR
	Qt5Network_DIR
	Qt5Qml_DIR
	Qt5Quick_DIR
	Qt5Test_DIR
	Qt5Widgets_DIR
	Qt5_DIR
	Qt5DBus_DIR
	GPGME_EXECUTABLE
	GLib_LIBRARY
	LibGit2_VERSION_HEADER
	libuv_VERSION_HEADER
	Qt5Svg_DIR)
