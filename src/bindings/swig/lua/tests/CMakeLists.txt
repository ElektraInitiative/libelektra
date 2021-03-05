set (LUA_MODULEPATH "${CMAKE_SWIG_OUTDIR}")

function (do_lua_test source)
	cmake_parse_arguments (ARG "KDB_TEST" "" "REQUIRED_PLUGINS" ${ARGN})

	foreach (plugin ${ARG_REQUIRED_PLUGINS})
		list (FIND REMOVED_PLUGINS ${plugin} plugin_index)
		if (plugin_index GREATER -1)
			return ()
		endif (plugin_index GREATER -1)
	endforeach (plugin ${ARG_REQUIRED_PLUGINS})

	get_filename_component (name ${source} NAME)
	add_test (NAME ${name} COMMAND ${LUA_EXECUTABLE} "${CMAKE_CURRENT_SOURCE_DIR}/${source}")

	set_property (TEST ${name} PROPERTY ENVIRONMENT "LUA_CPATH=${LUA_MODULEPATH}/?.so" "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")

	set_property (TEST ${name} PROPERTY LABELS memleak bindings)

	if (ARG_KDB_TEST)
		set_property (
			TEST ${name}
			APPEND
			PROPERTY LABELS kdbtests)
		set_property (
			TEST ${name}
			APPEND
			PROPERTY RUN_SERIAL TRUE)
	endif ()
endfunction (do_lua_test)

if (NOT (APPLE AND ENABLE_ASAN))
	do_lua_test (test_key.lua)
	do_lua_test (test_keyset.lua)
	do_lua_test (test_kdb.lua KDB_TEST)
	do_lua_test (test_gopts.lua KDB_TEST REQUIRED_PLUGINS gopts)
endif (NOT (APPLE AND ENABLE_ASAN))
