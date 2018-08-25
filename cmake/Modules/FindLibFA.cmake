# ~~~
# Try to find LibFA, usually bundled with Augeas
# Once done this will define
#  LIBFA_FOUND        - System has LibFA
#  LIBFA_INCLUDE_DIRS - The LibFA include directories
#  LIBFA_LIBRARIES    - The libraries needed to use LibFA
#  LIBFA_LIBRARY_DIRS - The directories containing the required libraries
# ~~~
find_library (LIBFA_LIBRARY "fa")
find_path (LIBFA_INCLUDE_DIR fa.h)

include (FindPackageHandleStandardArgs)

# handles the REQUIRED, QUIET and version-related arguments to find_package(). It also sets the LIBFA_FOUND variable.
find_package_handle_standard_args (LibFA DEFAULT_MSG LIBFA_LIBRARY LIBFA_INCLUDE_DIR)

set (LIBFA_LIBRARIES ${LIBFA_LIBRARY})
set (LIBFA_INCLUDE_DIRS ${LIBFA_INCLUDE_DIR})

foreach (LIBFA_LIB ${LIBFA_LIBRARIES})
	get_filename_component (LIBFA_LIB_DIR "${LIBFA_LIB}" DIRECTORY)
	if (LIBFA_LIBRARY_DIRS)
		set (LIBFA_LIBRARY_DIRS "${LIBFA_LIBRARY_DIRS};${LIBFA_LIB_DIR}")
	else (LIBFA_LIBRARY_DIRS)
		set (LIBFA_LIBRARY_DIRS "${LIBFA_LIB_DIR}")
	endif (LIBFA_LIBRARY_DIRS)
endforeach ()

# hide variables from the CMake GUI
mark_as_advanced (LIBFA_LIBRARY)
