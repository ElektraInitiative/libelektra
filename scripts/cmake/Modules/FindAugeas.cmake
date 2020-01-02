# ~~~
# Finds augeas and its libraries
# Uses the same semantics as pkg_check_modules, i.e. LIBAUGEAS{_FOUND,_INCLUDE_DIR,_LIBRARIES}
#
# This is an adapted version of FindSystemd.cmake:
# Copyright: Red Hat, Inc. 2013
# Author: Martin Briza <mbriza@redhat.com>
#
# Distributed under the BSD license. See COPYING-CMAKE-SCRIPTS for details.
# ~~~

if (LIBAUGEAS_INCLUDE_DIR
    AND LIBAUGEAS_LIBRARIES
    AND LIBAUGEAS_PREFIX) # in cache already
	set (LIBAUGEAS_FOUND TRUE)
else (LIBAUGEAS_INCLUDE_DIR)

	# try to find libaugeas via pkg-config
	find_package (PkgConfig QUIET)

	if (PKG_CONFIG_FOUND)
		pkg_check_modules (_LIBAUGEAS_PC QUIET "libaugeas")
	endif (PKG_CONFIG_FOUND)

	find_path (LIBAUGEAS_INCLUDE_DIR augeas.h ${_LIBAUGEAS_PC_INCLUDE_DIRS} /usr/include /usr/local/include)

	if (_LIBAUGEAS_PC_FOUND)
		pkg_get_variable (_LIBAUGEAS_PREFIX augeas prefix)
	endif (_LIBAUGEAS_PC_FOUND)

	if (NOT _LIBAUGEAS_PREFIX)
		if (APPLE)
			set (_LIBAUGEAS_PREFIX "/usr/local")
		else (APPLE)
			set (_LIBAUGEAS_PREFIX "/usr")
		endif (APPLE)
	endif ()
	set (
		LIBAUGEAS_PREFIX
		"${_LIBAUGEAS_PREFIX}"
		CACHE INTERNAL "prefix path of libaugeas" FORCE)

	find_library (
		LIBAUGEAS_LIBRARIES
		NAMES augeas
		PATHS ${_LIBAUGEAS_PC_LIBDIR})

	if (LIBAUGEAS_INCLUDE_DIR AND LIBAUGEAS_LIBRARIES)
		set (LIBAUGEAS_FOUND TRUE)
	endif (LIBAUGEAS_INCLUDE_DIR AND LIBAUGEAS_LIBRARIES)

	if (LIBAUGEAS_FOUND)
		if (NOT Augeas_FIND_QUIETLY)
			message (STATUS "Found augeas: ${LIBAUGEAS_LIBRARIES}")
		endif (NOT Augeas_FIND_QUIETLY)
	else (LIBAUGEAS_FOUND)
		if (LIBAUGEAS_FIND_REQUIRED)
			message (FATAL_ERROR "Could NOT find augeas")
		endif (LIBAUGEAS_FIND_REQUIRED)
	endif (LIBAUGEAS_FOUND)

	mark_as_advanced (LIBAUGEAS_INCLUDE_DIR LIBAUGEAS_LIBRARIES _LIBAUGEAS_PREFIX)

endif ()
