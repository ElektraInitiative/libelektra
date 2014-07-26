cmake_minimum_required (VERSION 2.8)

project (Elektra)

#additional modules for loading libraries
set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")


#needed by ifs below
include (cmake/ElektraCache.cmake)
include (cmake/ElektraCompiling.cmake)
if (ENABLE_COVERAGE)
	include (cmake/ElektraCoverage.cmake)
endif (ENABLE_COVERAGE)

if (ENABLE_TESTING)
	enable_testing ()
	include (Dart)
endif (ENABLE_TESTING)

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
	if (NOT FORCE_IN_SOURCE_BUILD)
		message(FATAL_ERROR
				"In-source builds are not permitted.\n"
				"Make a separate folder for building:\n"
				"    mkdir build && cd build && cmake ..\n"
				"Before that, remove the files already created:\n"
				"    rm -rf CMakeCache.txt CMakeFiles\n"
				"If you really know what you are doing\n"
				"(will overwrite original files!) use:\n"
				"    cmake -DFORCE_IN_SOURCE_BUILD=ON\n"
			)
	endif (NOT FORCE_IN_SOURCE_BUILD)
endif(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)

if (NOT CMAKE_BUILD_TYPE)
	SET(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel." FORCE)
endif (NOT CMAKE_BUILD_TYPE)


#some basic and important variables
set (DOMAIN libelektra.org) #needed by doxygen
set (REVERSE_DOMAIN org.libelektra)

set (KDB_VERSION_MAJOR 0)
set (KDB_VERSION_MINOR 8)
set (KDB_VERSION_MICRO 7)

set (KDB_VERSION "${KDB_VERSION_MAJOR}.${KDB_VERSION_MINOR}.${KDB_VERSION_MICRO}")

set (SO_VERSION 4)
#set (SO_VERSION_TOOLS 3)

#needs version above
include (cmake/ElektraPackaging.cmake)

#now add all directories
add_subdirectory (cmake)
add_subdirectory (src)

if (BUILD_DOCUMENTATION)
	add_subdirectory (doc)
endif (BUILD_DOCUMENTATION)

if (BUILD_TESTING)
	if (BUILD_FULL OR BUILD_STATIC)
		add_subdirectory (tests)
	else (BUILD_FULL OR BUILD_STATIC)
		message(WARNING "Requested to build testing, but is impossible because neither full or static build active")
	endif (BUILD_FULL OR BUILD_STATIC)
endif (BUILD_TESTING)

if (BUILD_FULL OR BUILD_STATIC)
	if (BUILD_EXAMPLES)
		add_subdirectory (examples)
	else (BUILD_EXAMPLES)
		message(WARNING "Requested to build examples, but is impossible because neither full or static build active")
	endif (BUILD_EXAMPLES)
endif (BUILD_FULL OR BUILD_STATIC)

if (BUILD_FULL OR BUILD_STATIC)
	add_subdirectory (benchmarks EXCLUDE_FROM_ALL)
endif (BUILD_FULL OR BUILD_STATIC)

ADD_CUSTOM_TARGET(uninstall
	"${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_SOURCE_DIR}/cmake/ElektraUninstall.cmake")

configure_file(
	"${CMAKE_CURRENT_SOURCE_DIR}/scripts/make-source-package.in"
	"${CMAKE_CURRENT_BINARY_DIR}/scripts/make-source-package")

ADD_CUSTOM_TARGET(source-package
		COMMAND "${CMAKE_CURRENT_BINARY_DIR}/scripts/make-source-package"
		COMMENT "Make a elektra source package by using git"
		VERBATIM
		WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
		)
