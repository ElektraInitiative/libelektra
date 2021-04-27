# Try to find yaml-cpp
# ====================

# ~~~
# Once done this will define
#
#  yaml-cpp_FOUND		System has yaml-cpp
#  yaml-cpp_INCLUDE_DIRS	The yaml-cpp include directories
#  yaml-cpp_LIBRARIES		The libraries needed to use yaml-cpp
#  yaml-cpp_VERSION		The version string of yaml-cpp
#
# This script is a modified version of the code available here: https://cmake.org/Wiki/CMake:How_To_Find_Libraries#Writing_find_modules
# ~~~

find_package (PkgConfig QUIET)

if (PKG_CONFIG_FOUND)
	pkg_check_modules (PC_yaml-cpp QUIET yaml-cpp)
endif (PKG_CONFIG_FOUND)

find_path (
	yaml-cpp_INCLUDE_DIR
	NAMES yaml-cpp/yaml.h
	HINTS ${PC_yaml-cpp_INCLUDEDIR} ${PC_yaml-cpp_INCLUDE_DIRS}
	PATH_SUFFIXES yaml-cpp)

find_library (
	yaml-cpp_LIBRARY
	NAMES yaml-cpp
	HINTS ${PC_yaml-cpp_LIBDIR} ${PC_yaml-cpp_LIBRARY_DIRS})

set (yaml-cpp_VERSION ${PC_yaml-cpp_VERSION})

include (FindPackageHandleStandardArgs)
# Handle the QUIETLY and REQUIRED arguments and set yaml-cpp_FOUND to TRUE, if all listed variables are TRUE
find_package_handle_standard_args (
	yaml-cpp
	REQUIRED_VARS yaml-cpp_LIBRARY yaml-cpp_INCLUDE_DIR
	VERSION_VAR yaml-cpp_VERSION)

mark_as_advanced (yaml-cpp_INCLUDE_DIR yaml-cpp_LIBRARY yaml-cpp_VERSION)

set (yaml-cpp_LIBRARIES ${yaml-cpp_LIBRARY})
set (yaml-cpp_INCLUDE_DIRS ${yaml-cpp_INCLUDE_DIR})
