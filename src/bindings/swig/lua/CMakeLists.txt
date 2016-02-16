include (${SWIG_USE_FILE})
include(LibAddMacros)

find_package (Lua 5.1)

if (NOT LUA_FOUND)
	remove_binding(swig_lua "Lua libs >= 5.2 not found")
else()
	message(STATUS "Include Binding swig_lua")

	add_headers (HDR_FILES)
	add_cppheaders (HDR_FILES)

	include_directories (${LUA_INCLUDE_DIR})
	include_directories (${CMAKE_CURRENT_SOURCE_DIR})

	set (CMAKE_SWIG_OUTDIR "${CMAKE_CURRENT_BINARY_DIR}")
	set (CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_SWIG_OUTDIR}")
	set (CMAKE_SWIG_FLAGS "-O")

	set_source_files_properties (kdb.i PROPERTIES CPLUSPLUS ON)
	swig_add_module (swig-lua lua kdb.i)
	swig_link_libraries (swig-lua elektra-core elektra-kdb ${LUA_LIBRARIES})
	set_target_properties (swig-lua PROPERTIES OUTPUT_NAME kdb)
	set_target_properties (${SWIG_MODULE_swig-lua_REAL_NAME} PROPERTIES PREFIX "")

	set_source_files_properties (${swig_generated_file_fullname} PROPERTIES COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -DSWIG_TYPE_TABLE=kdb")

	install (
		TARGETS swig-lua
		LIBRARY DESTINATION ${TARGET_LUA_CMOD_FOLDER}
	)

	include_directories (${CMAKE_CURRENT_SOURCE_DIR})
	include_directories (${CMAKE_SOURCE_DIR}/src/swig/)

	add_subdirectory (tests)
endif()
