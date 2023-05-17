cmake_minimum_required (VERSION 3.12)

# allows us to override platform specific variables
set (CMAKE_USER_MAKE_RULES_OVERRIDE "${CMAKE_SOURCE_DIR}/scripts/cmake/Platform.cmake")

set (KDB_VERSION_MAJOR 0)
set (KDB_VERSION_MINOR 10)
set (KDB_VERSION_PATCH 0)
set (KDB_VERSION "${KDB_VERSION_MAJOR}.${KDB_VERSION_MINOR}.${KDB_VERSION_PATCH}")

set (ELEKTRA_DESCRIPTION
     "Elektra serves as a universal and secure framework to access configuration settings in a global, hierarchical key database.")
set (ELEKTRA_HOMEPAGE_URL "https://www.libelektra.org")

project (
	Elektra
	VERSION ${KDB_VERSION}
	DESCRIPTION ${ELEKTRA_DESCRIPTION}
	HOMEPAGE_URL ${ELEKTRA_HOMEPAGE_URL})

# fix macOS RPATH issues
if (APPLE)
	set (CMAKE_MACOSX_RPATH 1)
	set (CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
endif (APPLE)

# use build RPATH for tests
set (CMAKE_BUILD_RPATH "${CMAKE_BINARY_DIR}/lib")

# when building, don't use the install RPATH already (but later on when installing)
set (CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)

# add the automatically determined parts of the RPATH which point to directories outside the build tree to the install RPATH
set (CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

set (CMAKE_INSTALL_DEFAULT_COMPONENT_NAME "elektra-misc")

# additional modules for loading libraries
set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/scripts/cmake/Modules/")

file (REMOVE "${CMAKE_BINARY_DIR}/extra_install_manifest.txt")
file (REMOVE "${CMAKE_BINARY_DIR}/external-links.txt")

# needed by ifs below
include (scripts/cmake/ElektraCache.cmake)
include (scripts/cmake/ElektraCompiling.cmake)
if (ENABLE_COVERAGE)
	include (scripts/cmake/ElektraCoverage.cmake)
endif (ENABLE_COVERAGE)

if (ENABLE_TESTING)
	enable_testing ()
	include (Dart)
endif (ENABLE_TESTING)

if (CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
	if (NOT FORCE_IN_SOURCE_BUILD)
		message (
			FATAL_ERROR
				"In-source builds are not permitted.\n"
				"Make a separate folder for building:\n"
				"    mkdir build && cd build && cmake ..\n"
				"Before that, remove the files already created:\n"
				"    rm -rf CMakeCache.txt CMakeFiles\n"
				"If you really know what you are doing\n"
				"(will overwrite original files!) use:\n"
				"    cmake -DFORCE_IN_SOURCE_BUILD=ON\n")
	endif (NOT FORCE_IN_SOURCE_BUILD)
endif (CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)

if (NOT CMAKE_BUILD_TYPE)
	set (
		CMAKE_BUILD_TYPE
		RelWithDebInfo
		CACHE STRING "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel." FORCE)
endif (NOT CMAKE_BUILD_TYPE)

set (CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set (CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# some basic and important variables
set (DOMAIN libelektra.org) # needed by doxygen
set (REVERSE_DOMAIN org.libelektra)

message (STATUS "You are building Elektra ${KDB_VERSION}")

set (SO_VERSION 5)
set (SO_VERSION_TOOLS 2)
set (SO_VERSION_GETENV 0)

# ensure KDB_DB_SYSTEM is an absolute path
if (NOT IS_ABSOLUTE "${KDB_DB_SYSTEM}")
	set (KDB_DB_SYSTEM "${CMAKE_INSTALL_PREFIX}/${KDB_DB_SYSTEM}")
endif ()

# process KDB_DB_SPEC so it is available everywhere
if (NOT IS_ABSOLUTE "${KDB_DB_SPEC}")
	set (KDB_DB_SPEC "${CMAKE_INSTALL_PREFIX}/${KDB_DB_SPEC}")
endif ()

# let cmake folder be included before doc
add_subdirectory (scripts/cmake)

if (BUILD_DOCUMENTATION)
	add_subdirectory (doc)
endif (BUILD_DOCUMENTATION)

# is there anything to build except documentation?
if (BUILD_FULL
    OR BUILD_STATIC
    OR BUILD_SHARED)
	if (BUILD_TESTING)
		find_package (GTest)
	endif (BUILD_TESTING)

	add_subdirectory (src)
	add_subdirectory (examples)

	if (BUILD_TESTING)
		add_subdirectory (benchmarks)
		add_subdirectory (tests)
	endif (BUILD_TESTING)
endif ()

# add scripts after doc and src (without scripts/cmake) adding scripts last allows scripts to be included/excluded based on ADDED_BINDINGS,
# ADDED_PLUGINS, ...
add_subdirectory (scripts)

install (
	FILES LICENSE.md
	DESTINATION ${TARGET_DOCUMENTATION_TEXT_FOLDER}
	COMPONENT libelektra${SO_VERSION})

# needs to be included last to collect all install components
include (scripts/cmake/ElektraPackaging.cmake)
