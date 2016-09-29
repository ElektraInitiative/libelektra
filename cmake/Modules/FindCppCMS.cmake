# Find the cppcms includes and library and the booster include and library
#
#  CPPCMS_INCLUDE_DIR  - Where to find cppcms include sub-directory.
#  CPPCMS_LIBRARIES    - List of libraries when using CPPCMS.
#  CPPCMS_FOUND        - True if CPPCMS found.
#  BOOSTER_INCLUDE_DIR - Where to find booster include sub-directory.
#  BOOSTER_LIBRARIES   - List of libraries when using BOOSTER.
#  BOOSTER_FOUND       - True if BOOSTER found.

if (CPPCMS_INCLUDE_DIR)
  # Already in cache, be silent.
  set (CPPCMS_FIND_QUIETLY TRUE)
endif (CPPCMS_INCLUDE_DIR)

find_path (CPPCMS_INCLUDE_DIR cppcms/application.h PATHS /usr/include /usr/local/include)
find_path (BOOSTER_INCLUDE_DIR booster/assert.h PATHS /usr/include /usr/local/include)

find_library (CPPCMS_LIBRARY NAMES cppcms PATHS /usr/lib /usr/lib64 /usr/local/lib)
find_library (BOOSTER_LIBRARY NAMES booster PATHS /usr/lib /usr/lib64 /usr/local/lib)


# Handle the QUIETLY and REQUIRED arguments and set CPPCMS_FOUND to
# TRUE if all listed variables are TRUE.
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (
  CppCMS DEFAULT_MSG
  CPPCMS_LIBRARY CPPCMS_INCLUDE_DIR
)

find_package_handle_standard_args (
  BOOSTER DEFAULT_MSG
  BOOSTER_LIBRARY BOOSTER_INCLUDE_DIR
)

if (CPPCMS_FOUND)
  set (CPPCMS_LIBRARIES ${CPPCMS_LIBRARY} ${BOOSTER_LIBRARY})
else (CPPCMS_FOUND)
  set (CPPCMS_LIBRARIES)
endif (CPPCMS_FOUND)

mark_as_advanced (CPPCMS_LIBRARY CPPCMS_INCLUDE_DIR BOOSTER_LIBRARY BOOSTER_INCLUDE_DIR)
