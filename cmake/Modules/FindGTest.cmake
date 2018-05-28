# ~~~
# This script reads the GTEST_ROOT variable, if available,
# looking into it as base directory of the Google Test sources.
# ~~~

if (NOT GOOGLETEST_ROOT)
	# if GTEST_ROOT is not set but ENV GTEST_ROOT exists use the ENV var
	if (GTEST_ROOT STREQUAL "")
		if (NOT ($ENV{GTEST_ROOT} STREQUAL ""))
			set (GTEST_ROOT "$ENV{GTEST_ROOT}")
		endif ()
	endif ()
	if (NOT (GTEST_ROOT STREQUAL ""))
		if (EXISTS ${GTEST_ROOT}/CMakeLists.txt)
			set (GOOGLETEST_ROOT "${GTEST_ROOT}" CACHE INTERNAL "Path to a local copy of Google Test" FORCE)
			message (STATUS "Use Google Test framework located at “${GOOGLETEST_ROOT}”")
		endif ()
	endif ()
endif ()
