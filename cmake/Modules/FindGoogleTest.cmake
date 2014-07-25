# - Try to find the Google Test sources.
# Once done, this will define
#
#  GOOGLETEST_FOUND - system has GoogleTest sources
#  GOOGLETEST_ROOT - the root of the GoogleTest sources
#
# This script reads the GTEST_ROOT variable, which must be passed,
# looking into it as base directory of the GoogleTest sources.

if(GOOGLETEST_ROOT)
	set(GOOGLETEST_FOUND TRUE)
else()
	if(NOT (GTEST_ROOT STREQUAL ""))
		if(EXISTS ${GTEST_ROOT}/CMakeLists.txt)
			set(GOOGLETEST_ROOT "${GTEST_ROOT}"
				CACHE FILEPATH "The root of the GoogleTest sources")
		endif()
	endif()
	include(FindPackageHandleStandardArgs)
	find_package_handle_standard_args(GoogleTest REQUIRED_VARS GOOGLETEST_ROOT)
endif()

