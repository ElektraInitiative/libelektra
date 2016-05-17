if (DEPENDENCY_PHASE)
	find_package(Lua 5.1)
	find_swig()

	if (LUA_FOUND AND SWIG_FOUND)
		include (LibAddMacros)

		add_custom_command (OUTPUT runtime.h
			COMMAND ${SWIG_EXECUTABLE} -c++ -lua -external-runtime runtime.h
		)

		# we call this SWIG_COMPILE_FLAGS because we have the same variable in our swig bindings
		set (SWIG_COMPILE_FLAGS "-Wno-shadow -Wno-old-style-cast -Wno-unused-variable")
		set_source_files_properties (lua.cpp PROPERTIES COMPILE_FLAGS "${SWIG_COMPILE_FLAGS}")
	elseif (NOT LUA_FOUND)
		remove_plugin (lua "Lua libs >= 5.2 not found")
	else ()
		remove_plugin (lua "swig not found")
	endif ()
endif ()

add_plugin (lua
	CPP
	SOURCES
		lua.hpp lua.cpp ${CMAKE_CURRENT_BINARY_DIR}/runtime.h
	INCLUDE_DIRECTORIES
		${LUA_INCLUDE_DIR}
	LINK_LIBRARIES
		${LUA_LIBRARIES}
	COMPILE_DEFINITIONS
		SWIG_TYPE_TABLE=kdb
	INSTALL_TEST_DATA
)

if (ADDTESTING_PHASE)
	# bindings are required for tests
	list (FIND BINDINGS "swig_lua" FINDEX)
	if (BUILD_TESTING AND FINDEX GREATER -1)
		# test environment clears env so setting LUA_CPATH is no option
		# we workaround this by changing the current directory to our bindings
		# output directory + per default lua loads modules from current directory
		add_plugintest (lua
			MEMLEAK
			WORKING_DIRECTORY
				${CMAKE_CURRENT_BINARY_DIR}/../../bindings/swig/lua/
			INSTALL_TEST_DATA
		)
	else ()
		message (WARNING "swig_lua bindings are required for testing, test deactivated")
	endif ()
endif ()
