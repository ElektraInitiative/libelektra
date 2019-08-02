# Try to find YAEP
# ================

# This find script defines the following variables:
#
# - YAEP_FOUND:         System has YAEP
# - YAEP_INCLUDE_DIRS:  The YAEP include directories
# - YAEP_LIBRARIES:     The library needed to use YAEP (C)
# - YAEP_LIBRARIES_CPP: The library needed to use YAEP (C++)
#
# .

find_path (YAEP_INCLUDE_DIR NAMES yaep.h PATH_SUFFIXES yaep)

find_library (YAEP_LIBRARY NAMES yaep)
find_library (YAEP_LIBRARY_CPP NAMES yaep++)

include (FindPackageHandleStandardArgs)
# Handle the QUIETLY and REQUIRED arguments and set YAEP_FOUND to TRUE, if all listed variables are TRUE
find_package_handle_standard_args (YAEP
				   REQUIRED_VARS
				   YAEP_LIBRARY
				   YAEP_LIBRARY_CPP
				   YAEP_INCLUDE_DIR)

mark_as_advanced (YAEP_INCLUDE_DIR YAEP_LIBRARY)

set (YAEP_LIBRARIES ${YAEP_LIBRARY})
set (YAEP_LIBRARIES_CPP ${YAEP_LIBRARY_CPP})
set (YAEP_INCLUDE_DIRS ${YAEP_INCLUDE_DIR})
