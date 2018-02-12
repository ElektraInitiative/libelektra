# Try to find the C++ runtime of ANTLR
# ====================================

# Once done this will define
#
#  ANTLR4CPP_FOUND		Tells us if the script was able to find the C++ runtime of ANTLR
#  ANTLR4CPP_INCLUDE_DIRS	The ANTLR C++ runtime include directories
#  ANTLR4CPP_LIBRARIES		The libraries needed to use the ANTLR C++ runtime
#
# This script is a modified version of the code available here:
#	https://cmake.org/Wiki/CMake:How_To_Find_Libraries#Writing_find_modules

find_path (ANTLR4CPP_INCLUDE_DIR NAMES antlr4-runtime.h PATH_SUFFIXES antlr4-runtime)
find_library (ANTLR4CPP_LIBRARY NAMES antlr4-runtime)

include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (ANTLR4CPP DEFAULT_MSG ANTLR4CPP_LIBRARY ANTLR4CPP_INCLUDE_DIR)

mark_as_advanced (ANTLR4CPP_INCLUDE_DIR ANTLR4CPP_LIBRARY)

set (ANTLR4CPP_LIBRARIES ${ANTLR4CPP_LIBRARY})
set (ANTLR4CPP_INCLUDE_DIRS ${ANTLR4CPP_INCLUDE_DIR})
