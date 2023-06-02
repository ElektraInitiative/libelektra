include (${SWIG_USE_FILE})
include (LibAddMacros)

find_package (Lua 5.1 QUIET)

if (NOT LUA_FOUND)
	exclude_binding (lua "Lua libs >= 5.1 not found")
else ()
	if (NOT BUILD_SHARED)
		exclude_binding (lua "it can only be built if `BUILD_SHARED` is enabled")
		return ()
	endif (NOT BUILD_SHARED)

	add_binding (lua)

	add_headers (HDR_FILES)
	add_cppheaders (HDR_FILES)

	include_directories (SYSTEM ${LUA_INCLUDE_DIR})
	include_directories ("${CMAKE_SOURCE_DIR}/src/bindings/cpp/include")

	set (CMAKE_SWIG_OUTDIR "${CMAKE_CURRENT_BINARY_DIR}")
	set (CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_SWIG_OUTDIR}")
	set (CMAKE_SWIG_FLAGS "-O")

	set_source_files_properties (kdb.i PROPERTIES CPLUSPLUS ON)
	if (CMAKE_VERSION VERSION_LESS 3.8)
		swig_add_module (swig-lua lua kdb.i)
	else (CMAKE_VERSION VERSION_LESS 3.8)
		swig_add_library (
			swig-lua
			LANGUAGE lua
			SOURCES kdb.i
			TYPE MODULE)
	endif (CMAKE_VERSION VERSION_LESS 3.8)
	swig_link_libraries (swig-lua elektra-core elektra-kdb ${LUA_LIBRARIES})
	set_target_properties (swig-lua PROPERTIES OUTPUT_NAME kdb)
	set_target_properties (${SWIG_MODULE_swig-lua_REAL_NAME} PROPERTIES PREFIX "")

	set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-ignored-qualifiers")
	set_source_files_properties (${swig_generated_file_fullname} PROPERTIES COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -DSWIG_TYPE_TABLE=kdb")

	install (TARGETS swig-lua LIBRARY DESTINATION ${TARGET_LUA_CMOD_FOLDER} COMPONENT lua-elektra)

	include_directories (${CMAKE_CURRENT_SOURCE_DIR})
	include_directories (${CMAKE_SOURCE_DIR}/src/swig/)

	if (LUA_EXECUTABLE_VERSION_MATCHED)
		if (BUILD_TESTING)
			add_subdirectory (tests)
		endif ()
	else ()
		message (WARNING "Lua interpreter does not match Lua libraries version, "
				 "needed for SWIG Lua bindings tests. Tests will not be executed.")
	endif ()
endif ()
