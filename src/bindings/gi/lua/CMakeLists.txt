macro (do_gi_lua_test source)
	get_filename_component (name ${source} NAME)
	add_test (
		NAME ${name}
		COMMAND lua ${source}
		WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	)

	set_property(
		TEST ${name}
		PROPERTY ENVIRONMENT
			"LD_LIBRARY_PATH=${GELEKTRA_LIBRARY_DIR}"
			"GI_TYPELIB_PATH=${GI_TYPELIB_DIR}"
	)
endmacro (do_gi_lua_test)

install (DIRECTORY lgi DESTINATION ${TARGET_LUA_LMOD_FOLDER})

# we're searching for the lua interpreter however FindLua52 only searches
# for the library. But that's better than nothing.
find_package (Lua52)
if (LUA52_FOUND)
	file (GLOB TESTS testgi_*.lua)
	foreach (file ${TESTS})
		do_gi_lua_test (${file})
	endforeach (file ${TESTS})
else (LUA52_FOUND)
	message (WARNING "No Lua interpreter found. Skipping GObject Introspection Lua tests")
endif (LUA52_FOUND)
