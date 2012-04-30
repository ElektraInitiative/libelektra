cmake_minimum_required (VERSION 2.6)

project (Elektra)

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
	message(FATAL_ERROR "In-source builds are not permitted. Make a separate folder for building:\nmkdir build; cd build; cmake ..\nBefore that, remove the files already created:\nrm -rf CMakeCache.txt CMakeFiles")
	if (NOT FORCE_IN_SOURCE_BUILD)
		message(FATAL_ERROR
				"In-source builds are not permitted.\n"
				"Make a separate folder for building:\n"
				"    mkdir build && cd build && cmake ..\n"
				"Before that, remove the files already created:\n"
				"    rm -rf CMakeCache.txt CMakeFiles\n"
				"If you really know what you are doing\n
				"(will overwrite original files!) use:\n"
				"    cmake -DFORCE_IN_SOURCE_BUILD=ON\n")
	endif (NOT FORCE_IN_SOURCE_BUILD)
endif(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)

if (ENABLE_TESTING)
	enable_testing ()
	include (Dart)
endif (ENABLE_TESTING)

#additional modules for loading libraries
set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

if (NOT CMAKE_BUILD_TYPE)
	SET(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel." FORCE)
endif (NOT CMAKE_BUILD_TYPE)

set (DOMAIN libelektra.org)
set (REVERSE_DOMAIN org.libelektra)

set (KDB_VERSION_MAJOR 0)
set (KDB_VERSION_MINOR 8)
set (KDB_VERSION_MICRO 0)
set (KDB_VERSION_POSTFIX rc1)

set (KDB_VERSION "${KDB_VERSION_MAJOR}.${KDB_VERSION_MINOR}.${KDB_VERSION_MICRO}${KDB_VERSION_POSTFIX}")
set (KDB_VERSION_MAJORMINOR "${KDB_VERSION_MAJOR}.${KDB_VERSION_MINOR}")

set (SO_VERSION 4)

include (cmake/ElektraPackaging.cmake)
include (cmake/ElektraCache.cmake)
include (cmake/ElektraCompiling.cmake)

add_subdirectory (src)
add_subdirectory (cmake)

if (BUILD_DOCUMENTATION)
	add_subdirectory (doc)
endif (BUILD_DOCUMENTATION)

if (BUILD_TESTING)
	if (BUILD_FULL OR BUILD_STATIC)
		add_subdirectory (tests)
	endif (BUILD_FULL OR BUILD_STATIC)
endif (BUILD_TESTING)

if (BUILD_FULL OR BUILD_STATIC)
	add_subdirectory (examples)
endif (BUILD_FULL OR BUILD_STATIC)
#add_subdirectory (examples EXCLUDE_FROM_ALL)

if (BUILD_FULL OR BUILD_STATIC)
	add_subdirectory (benchmarks)
endif (BUILD_FULL OR BUILD_STATIC)
#add_subdirectory (benchmarks EXCLUDE_FROM_ALL)

ADD_CUSTOM_TARGET(uninstall
	"${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_SOURCE_DIR}/cmake/ElektraUninstall.cmake")
