#
# Find libyajl
#
# yajl is a json parser needed for the plugin yajl
#
# YAJL_FOUND          - true if yajl was found
# YAJL_INCLUDE_DIRS   - where to find the header file yajl/yajl_common.h
# YAJL_LIBRARIES      - where to find the library libyail
#

find_path(YAJL_INCLUDE_DIR yajl/yajl_common.h)

set(YAJL_NAMES ${YAJL_NAMES} yajl libyajl)
find_library(YAJL_LIBRARY NAMES ${YAJL_NAMES} PATH)

INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(Yajl DEFAULT_MSG YAJL_LIBRARY YAJL_INCLUDE_DIR)

if(YAJL_FOUND)
	set (YAJL_INCLUDE_DIRS ${YAJL_INCLUDE_DIR})
	set (YAJL_LIBRARIES ${YAJL_LIBRARY})
endif(YAJL_FOUND)
