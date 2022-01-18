# Find Module for ZeroMQ
# ======================
#
# This module defines the variables listed below.
#
# - ZeroMQ_FOUND:        This variable will be set to a value that evaluates to true, if the system includes ZeroMQ.
# - ZeroMQ_INCLUDE_DIRS: This variable stores the ZeroMQ include directory.
# - ZeroMQ_LIBRARIES:    This variable stores the libraries needed to link to ZeroMQ.

find_package (PkgConfig QUIET)
if (PKG_CONFIG_FOUND)
	pkg_check_modules (PC_ZeroMQ QUIET libzmq)
endif (PKG_CONFIG_FOUND)

find_path (
	ZeroMQ_INCLUDE_DIR
	NAMES zmq.h
	HINTS ${PC_ZeroMQ_INCLUDEDIR} ${PC_ZeroMQ_INCLUDE_DIRS})
find_library (
	ZeroMQ_LIBRARY
	NAMES zmq
	HINTS ${PC_ZeroMQ_LIBDIR} ${PC_ZeroMQ_LIBRARY_DIRS})

set (ZeroMQ_VERSION 0)
set (ZeroMQ_VERSION_HEADER ${ZeroMQ_INCLUDE_DIR}/zmq.h)
if (EXISTS ${ZeroMQ_VERSION_HEADER})

	file (
		STRINGS ${ZeroMQ_VERSION_HEADER} ZeroMQ_VERSION_MAJOR
		REGEX "#define ZMQ_VERSION_MAJOR [0-9]+"
		LIMIT_COUNT 1)
	string (REGEX REPLACE "[^0-9]*([0-9]+)$" "\\1" ZeroMQ_VERSION_MAJOR "${ZeroMQ_VERSION_MAJOR}")

	file (
		STRINGS ${ZeroMQ_VERSION_HEADER} ZeroMQ_VERSION_MINOR
		REGEX "#define ZMQ_VERSION_MINOR [0-9]+"
		LIMIT_COUNT 1)
	string (REGEX REPLACE "[^0-9]*([0-9]+)$" "\\1" ZeroMQ_VERSION_MINOR "${ZeroMQ_VERSION_MINOR}")

	file (
		STRINGS ${ZeroMQ_VERSION_HEADER} ZeroMQ_VERSION_PATCH
		REGEX "#define ZMQ_VERSION_PATCH [0-9]+"
		LIMIT_COUNT 1)
	string (REGEX REPLACE "[^0-9]*([0-9]+)$" "\\1" ZeroMQ_VERSION_PATCH "${ZeroMQ_VERSION_PATCH}")

	set (ZeroMQ_VERSION "${ZeroMQ_VERSION_MAJOR}.${ZeroMQ_VERSION_MINOR}.${ZeroMQ_VERSION_PATCH}")
endif (EXISTS ${ZeroMQ_VERSION_HEADER})

include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (
	ZeroMQ
	REQUIRED_VARS ZeroMQ_LIBRARY ZeroMQ_INCLUDE_DIR
	VERSION_VAR ZeroMQ_VERSION)

mark_as_advanced (ZeroMQ_INCLUDE_DIR ZeroMQ_LIBRARY ZeroMQ_VERSION)

set (ZeroMQ_LIBRARIES ${ZeroMQ_LIBRARY})
set (ZeroMQ_INCLUDE_DIRS ${ZeroMQ_INCLUDE_DIR})
