# Finds the Google Test sources.
# Once done, this will define
#
#  GOOGLETEST_FOUND - GoogleTest sources were found
#  GOOGLETEST_ROOT - the root of the GoogleTest sources
#
# If ENABLE_EXTERNAL_GTEST is set,
# this script reads the GTEST_ROOT variable, which must be passed,
# looking into it as base directory of the GoogleTest sources.
#
# Otherwise it will use the source as included with Elektra.

if (ENABLE_EXTERNAL_GTEST)
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
else ()
	set(GOOGLETEST_FOUND TRUE)
	set(GOOGLETEST_ROOT ${CMAKE_SOURCE_DIR}/src/external/gtest-1.7.0)
endif ()

