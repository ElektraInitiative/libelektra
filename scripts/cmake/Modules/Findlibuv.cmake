# Find Module for libuv
# =====================
#
# This module defines the variables listed below.
#
# - libuv_FOUND:        This variable will be set to a value that evaluates to true, if the system includes libuv.
# - libuv_INCLUDE_DIRS: This variable stores the libuv include directory.
# - libuv_LIBRARIES:    This variable stores the libraries needed to link to libuv.

find_package (PkgConfig QUIET)
if (PKG_CONFIG_FOUND)
	pkg_check_modules (PC_libuv QUIET libuv)
endif (PKG_CONFIG_FOUND)

find_path (
	libuv_INCLUDE_DIR
	NAMES uv.h
	HINTS ${PC_libuv_INCLUDEDIR} ${PC_libuv_INCLUDE_DIRS})
find_library (
	libuv_LIBRARY
	NAMES uv
	HINTS ${PC_libuv_LIBDIR} ${PC_libuv_LIBRARY_DIRS})

set (libuv_VERSION 0)
find_file (libuv_VERSION_HEADER NAMES uv/version.h uv-version.h)
if (NOT libuv_VERSION_HEADER STREQUAL libuv_VERSION_HEADER-NOTFOUND)

	file (
		STRINGS ${libuv_VERSION_HEADER} libuv_VERSION_MAJOR
		REGEX "#define UV_VERSION_MAJOR [0-9]+"
		LIMIT_COUNT 1)
	string (REGEX REPLACE "[^0-9]*([0-9]+)$" "\\1" libuv_VERSION_MAJOR ${libuv_VERSION_MAJOR})

	file (
		STRINGS ${libuv_VERSION_HEADER} libuv_VERSION_MINOR
		REGEX "#define UV_VERSION_MINOR [0-9]+"
		LIMIT_COUNT 1)
	string (REGEX REPLACE "[^0-9]*([0-9]+)$" "\\1" libuv_VERSION_MINOR ${libuv_VERSION_MINOR})

	file (
		STRINGS ${libuv_VERSION_HEADER} libuv_VERSION_PATCH
		REGEX "#define UV_VERSION_PATCH [0-9]+"
		LIMIT_COUNT 1)
	string (REGEX REPLACE "[^0-9]*([0-9]+)$" "\\1" libuv_VERSION_PATCH ${libuv_VERSION_PATCH})

	set (libuv_VERSION "${libuv_VERSION_MAJOR}.${libuv_VERSION_MINOR}.${libuv_VERSION_PATCH}")
endif (NOT libuv_VERSION_HEADER STREQUAL libuv_VERSION_HEADER-NOTFOUND)

include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (
	libuv
	REQUIRED_VARS libuv_LIBRARY libuv_INCLUDE_DIR
	VERSION_VAR libuv_VERSION)

mark_as_advanced (libuv_INCLUDE_DIR libuv_LIBRARY libuv_VERSION)

set (libuv_LIBRARIES ${libuv_LIBRARY})
set (libuv_INCLUDE_DIRS ${libuv_INCLUDE_DIR})
