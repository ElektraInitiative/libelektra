# Try to find yaml-cpp
# ====================

# ~~~
# Once done this will define
#
#  YAML-CPP_FOUND		System has yaml-cpp
#  YAML-CPP_INCLUDE_DIRS	The yaml-cpp include directories
#  YAML-CPP_LIBRARIES		The libraries needed to use yaml-cpp
#  YAML-CPP_VERSION		The version string of yaml-cpp
#
# This script is a modified version of the code available here: https://cmake.org/Wiki/CMake:How_To_Find_Libraries#Writing_find_modules
# ~~~

find_package (PkgConfig QUIET)

if (PKG_CONFIG_FOUND)
	pkg_check_modules (PC_YAML-CPP QUIET yaml-cpp)
endif (PKG_CONFIG_FOUND)

find_path (YAML-CPP_INCLUDE_DIR NAMES yaml-cpp/yaml.h HINTS ${PC_YAML-CPP_INCLUDEDIR} ${PC_YAML-CPP_INCLUDE_DIRS} PATH_SUFFIXES yaml-cpp)

find_library (YAML-CPP_LIBRARY NAMES yaml-cpp HINTS ${PC_YAML-CPP_LIBDIR} ${PC_YAML-CPP_LIBRARY_DIRS})

set (YAML-CPP_VERSION ${PC_YAML-CPP_VERSION})

include (FindPackageHandleStandardArgs)
# Handle the QUIETLY and REQUIRED arguments and set YAML-CPP_FOUND to TRUE, if all listed variables are TRUE
find_package_handle_standard_args (yaml-cpp
				   REQUIRED_VARS
				   YAML-CPP_LIBRARY
				   YAML-CPP_INCLUDE_DIR
				   VERSION_VAR
				   YAML-CPP_VERSION)

mark_as_advanced (YAML-CPP_INCLUDE_DIR YAML-CPP_LIBRARY YAML-CPP_VERSION)

set (YAML-CPP_LIBRARIES ${YAML-CPP_LIBRARY})
set (YAML-CPP_INCLUDE_DIRS ${YAML-CPP_INCLUDE_DIR})
