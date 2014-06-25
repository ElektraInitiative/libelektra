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

if (NOT LUA_CPATH)
	execute_process (
		COMMAND lua -e "for k in string.gmatch(package.cpath .. \";\", \"([^;]+)/..so;\") do if k:sub(1,1) == \"/\" then print(k) break end end"
		OUTPUT_VARIABLE LUA_CPATH
		RESULT_VARIABLE LUA_CHECK_RES
		OUTPUT_STRIP_TRAILING_WHITESPACE
	)

	if (NOT ${LUA_CHECK_RES} EQUAL 0 OR ${LUA_CPATH} EQUAL "")
		message (SEND_ERROR "Lua binary was not found on your system")
	endif ()
endif ()

if (NOT LUA_PATH)
	execute_process (
		COMMAND  lua -e "for k in string.gmatch(package.path .. \";\", \"([^;]+)/..lua;\") do if k:sub(1,1) == \"/\" then print(k) break end end"
		OUTPUT_VARIABLE LUA_PATH
		RESULT_VARIABLE LUA_CHECK_RES
		OUTPUT_STRIP_TRAILING_WHITESPACE
	)

	if (NOT ${LUA_CHECK_RES} EQUAL 0 OR ${LUA_PATH} EQUAL "")
		message (SEND_ERROR "Lua binary was not found on your system")
	endif ()
endif ()

install (
	TARGETS swig-lua
	LIBRARY DESTINATION ${LUA_CPATH}
)

include_directories (${CMAKE_CURRENT_SOURCE_DIR})
include_directories (${CMAKE_SOURCE_DIR}/src/swig/)

add_subdirectory (tests)
