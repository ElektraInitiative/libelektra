# ~~~
# Find libyajl
#
# yajl is a json parser needed for the plugin yajl
#
# YAJL_FOUND          - true if yajl was found
# YAJL_INCLUDE_DIRS   - where to find the header file yajl/yajl_common.h
# YAJL_LIBRARIES      - where to find the library libyail
# YAJL_VERSION        - "1" if no yajl/yajl_version.h found
# ~~~

find_path (YAJL_INCLUDE_DIR yajl/yajl_common.h)

set (YAJL_NAMES ${YAJL_NAMES} yajl libyajl)
find_library (YAJL_LIBRARY NAMES ${YAJL_NAMES})

include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (Yajl DEFAULT_MSG YAJL_LIBRARY YAJL_INCLUDE_DIR)

if (YAJL_FOUND)
	set (YAJL_INCLUDE_DIRS ${YAJL_INCLUDE_DIR})
	set (YAJL_LIBRARIES ${YAJL_LIBRARY})

	find_path (YAJL2_INCLUDE_DIR yajl/yajl_version.h)
	if (NOT YAJL2_INCLUDE_DIR)
		message (STATUS "Assume Yajl Version 1 because yajl/yajl_version.h was not found")
		set (YAJL_NO_VERSION "1")
	endif (NOT YAJL2_INCLUDE_DIR)
endif (YAJL_FOUND)

mark_as_advanced (YAJL2_INCLUDE_DIR YAJL_INCLUDE_DIR YAJL_LIBRARY)
