include (${SWIG_USE_FILE})
include(LibAddMacros)

find_package (Lua52 REQUIRED)

add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)

include_directories (${LUA_INCLUDE_DIR})
include_directories (${CMAKE_CURRENT_SOURCE_DIR})

set (CMAKE_SWIG_OUTDIR ${CMAKE_CURRENT_BINARY_DIR})
set (CMAKE_SWIG_FLAGS "-O")

set_source_files_properties (kdb.i PROPERTIES CPLUSPLUS ON)
swig_add_module (swig-lua lua kdb.i)
swig_link_libraries (swig-lua elektra ${LUA_LIBRARIES})
set_target_properties (swig-lua PROPERTIES OUTPUT_NAME kdb)
set_target_properties (${SWIG_MODULE_swig-lua_REAL_NAME} PROPERTIES PREFIX "")

install (
	TARGETS swig-lua
	LIBRARY DESTINATION ${TARGET_LUA_CMOD_FOLDER}
)

include_directories (${CMAKE_CURRENT_SOURCE_DIR})
include_directories (${CMAKE_SOURCE_DIR}/src/swig/)

add_subdirectory (tests)
