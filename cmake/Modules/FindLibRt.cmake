# ~~~
# Finds librt
# Sets the variables LIBRT_FOUND and LIBRT_LIBRARIES
#
# Distributed under the BSD license. See COPYING-CMAKE-SCRIPTS for details.
# ~~~

find_library (LIBRT_LIBRARIES "rt")

include (FindPackageHandleStandardArgs)

# handles the REQUIRED, QUIET and version-related arguments to find_package(). It also sets the LIBRT_FOUND variable.
find_package_handle_standard_args (LibRt DEFAULT_MSG LIBRT_LIBRARIES)

# hide variables from the CMake GUI
mark_as_advanced (LIBRT_LIBRARIES)

# Most systems (except Linux) have librt features built into their libc. If no librt has been found, there is still a chance that the system
# supports our desired features.
if (NOT LIBRT_FOUND)
	try_compile (HAS_LIBRT_4SURE "${CMAKE_BINARY_DIR}" "${PROJECT_SOURCE_DIR}/tests/librt_test.c")

	if (HAS_LIBRT_4SURE)
		set (LIBRT_FOUND on)
		set (LIBRT_LIBRARIES "")
		message (STATUS "using system's built-in librt functions")
	endif ()
	unset (HAS_LIBRT_4SURE)
endif ()
