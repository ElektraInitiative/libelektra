# This script reads the GTEST_ROOT variable, if available,
# looking into it as base directory of the GoogleTest sources.
#
# Otherwise it will use the source as included with Elektra.
#
# Must be included only once
#
# GTEST_ROOT will only be honored in the very first invocation

if (NOT GOOGLETEST_ROOT)
	if (NOT (GTEST_ROOT STREQUAL ""))
		if (EXISTS ${GTEST_ROOT}/CMakeLists.txt)
			set(GOOGLETEST_ROOT
				"${GTEST_ROOT}"
				CACHE INTERNAL
				"Folder which gtest will use"
				FORCE)
			message (STATUS "used external gtest below ${GTEST_ROOT}")
		endif()
	else()
		set(GOOGLETEST_ROOT
			${CMAKE_SOURCE_DIR}/tests/gtest-1.7.0
			CACHE INTERNAL
			"Folder which gtest will use"
			FORCE)
	endif()
endif()

add_subdirectory(${GOOGLETEST_ROOT} ${CMAKE_BINARY_DIR}/gtest)

set_property (TARGET gtest PROPERTY COMPILE_FLAGS "-w" )
set_property (TARGET gtest_main PROPERTY COMPILE_FLAGS "-w" )
