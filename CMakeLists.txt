cmake_minimum_required (VERSION 2.6)

project (Elektra)

enable_testing ()
include (Dart)

#additional modules for loading libraries
set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

if(NOT CMAKE_BUILD_TYPE)
	SET(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel." FORCE)
endif(NOT CMAKE_BUILD_TYPE)

set (KDB_VERSION_MAJOR 0)
set (KDB_VERSION_MINOR 8)
set (KDB_VERSION_MICRO 0)

set (KDB_VERSION "${KDB_VERSION_MAJOR}.${KDB_VERSION_MINOR}.${KDB_VERSION_MICRO}")
set (KDB_VERSION_MAJORMINOR "${KDB_VERSION_MAJOR}.${KDB_VERSION_MINOR}")
set (PROJECT_VERSION "${KDB_VERSION}")

include (cmake/ElektraPackaging.cmake)
include (cmake/ElektraCache.cmake)
include (cmake/ElektraCompiling.cmake)

add_subdirectory(src)
add_subdirectory(tests)
add_subdirectory(cmake)

