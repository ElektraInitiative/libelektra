# Finds librt
# Sets the variables LIBRT_FOUND and LIBRT_LIBRARIES
#
# Distributed under the BSD license. See COPYING-CMAKE-SCRIPTS for details.

find_library (LIBRT_LIBRARIES "rt")

include (FindPackageHandleStandardArgs)

# handles the REQUIRED, QUIET and version-related arguments to find_package(). 
# It also sets the LIBRT_FOUND variable.
find_package_handle_standard_args (LibRt DEFAULT_MSG LIBRT_LIBRARIES)

# hide variables from the CMake GUI
mark_as_advanced (LIBRT_LIBRARIES)