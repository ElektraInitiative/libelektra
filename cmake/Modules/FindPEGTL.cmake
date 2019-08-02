# Try to find PEGTL
# =================

# This find script defines the following variables:
#
# - `PEGTL_FOUND`:         System has PEGTL
# - `PEGTL_INCLUDE_DIRS`:  The PEGTL include directories
# - `PEGTL_VERSION`:       The PEGTL version
#
# .

find_path (PEGTL_INCLUDE_DIR NAMES tao/pegtl.hpp PATH_SUFFIXES tao)

set (PEGTL_VERSION 0)
if (EXISTS "${PEGTL_INCLUDE_DIR}/tao/pegtl/version.hpp")
	file (STRINGS "${PEGTL_INCLUDE_DIR}/tao/pegtl/version.hpp" PEGTL_VERSION REGEX "#define TAO_PEGTL_VERSION \".+\"" LIMIT_COUNT 1)
	string (REGEX
		REPLACE ".*\"(.+)\""
			"\\1"
			PEGTL_VERSION
			${PEGTL_VERSION})
endif (EXISTS "${PEGTL_INCLUDE_DIR}/tao/pegtl/version.hpp")

include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (PEGTL
				   REQUIRED_VARS
				   PEGTL_INCLUDE_DIR
				   VERSION_VAR
				   PEGTL_VERSION)

mark_as_advanced (PEGTL_INCLUDE_DIR)
set (PEGTL_INCLUDE_DIRS ${PEGTL_INCLUDE_DIR})
