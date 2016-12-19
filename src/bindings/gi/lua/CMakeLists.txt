macro (do_gi_lua_test source)
	get_filename_component (name ${source} NAME)
	add_test (
		NAME ${name}
		COMMAND ${LUA_EXECUTABLE} ${source}
		WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	)

	set_property (
		TEST ${name}
		PROPERTY ENVIRONMENT
			"LD_LIBRARY_PATH=${GELEKTRA_LIBRARY_DIR}:${CMAKE_BINARY_DIR}/lib"
			"GI_TYPELIB_PATH=${GI_TYPELIB_DIR}"
	)

	set_property (
		TEST ${name}
		PROPERTY LABELS memleak bindings
	)

	if (${name} MATCHES ".*_kdb")
		set_property (
			TEST ${name}
			APPEND PROPERTY LABELS kdbtests
		)
	endif ()
endmacro (do_gi_lua_test)

install (DIRECTORY lgi DESTINATION ${TARGET_LUA_LMOD_FOLDER})

# we're searching for the lua interpreter however FindLua only searches
# for the library. But that's better than nothing.
find_package (Lua 5.1)
if (LUA_FOUND AND LUA_EXECUTABLE_VERSION_MATCHED)
	message(STATUS "Include Binding gi_lua")
	message(DEPRECATION "GI bindings have been deprecated")

	file (GLOB TESTS testgi_*.lua)
	foreach (file ${TESTS})
		do_gi_lua_test (${file})
	endforeach (file ${TESTS})
elseif(LUA_FOUND AND (NOT LUA_EXECUTABLE_VERSION_MATCHED))
	remove_binding (gi_lua "Lua interpreter does not match Lua libraries version, needed for GObject Introspection Lua installation")
else ()
	remove_binding (gi_lua "No Lua interpreter found needed for GObject Introspection Lua installation")
endif ()
