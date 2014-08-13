set(LUA_MODULEPATH "${CMAKE_CURRENT_BINARY_DIR}/..")

macro (do_lua_test source)
	get_filename_component (name ${source} NAME)
	add_test (
		NAME ${name}
		COMMAND "lua" ${source}
		)
	set_tests_properties (${name} PROPERTIES ENVIRONMENT "LUA_CPATH=${LUA_MODULEPATH}/?.so")
	set_property(TEST ${name} PROPERTY LABELS memleak)
endmacro (do_lua_test)

file (GLOB TESTS *.lua)
foreach (file ${TESTS})
	do_lua_test (${file})
endforeach (file ${TESTS})
