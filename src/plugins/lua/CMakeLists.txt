include (LibAddBinding)

if (DEPENDENCY_PHASE)
	set (LUA_REQUIRED_VERSION 5.1)
	find_package (Lua ${LUA_REQUIRED_VERSION} QUIET)
	find_swig ()
	check_binding_included ("lua" BINDING_WAS_ADDED SUBDIRECTORY "swig/lua" SILENT)

	if (LUA_FOUND
	    AND SWIG_FOUND
	    AND BINDING_WAS_ADDED)
		include (LibAddMacros)

		add_custom_command (OUTPUT runtime.h COMMAND ${SWIG_EXECUTABLE} -c++ -lua -external-runtime runtime.h)

		# we call this SWIG_COMPILE_FLAGS because we have the same variable in our swig bindings
		set (SWIG_COMPILE_FLAGS
		     "-Wno-unknown-warning-option -Wno-shadow -Wno-old-style-cast -Wno-unused-variable -Wno-unused-but-set-variable")
		set_source_files_properties (lua.cpp PROPERTIES COMPILE_FLAGS "${SWIG_COMPILE_FLAGS}")
	elseif (NOT LUA_FOUND)
		remove_plugin (lua "Lua libs (>= liblua${LUA_REQUIRED_VERSION}-dev) not found")
	elseif (NOT BINDING_WAS_ADDED)
		remove_plugin (lua "lua binding is required")
	else ()
		remove_plugin (lua "swig not found")
	endif ()
endif ()

add_plugin (
	lua CPP
	SOURCES lua.hpp lua.cpp ${CMAKE_CURRENT_BINARY_DIR}/runtime.h
	INCLUDE_DIRECTORIES ${LUA_INCLUDE_DIR}
	LINK_LIBRARIES ${LUA_LIBRARIES}
	COMPILE_DEFINITIONS SWIG_TYPE_TABLE=kdb
	INSTALL_TEST_DATA COMPONENT libelektra${SO_VERSION}-lua)

if (ADDTESTING_PHASE)

	# TODO: ENABLE_BROKEN_TESTS
	if (APPLE
	    OR CMAKE_C_COMPILER_ID STREQUAL "Clang"
	    OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
		return ()
	endif ()

	check_binding_was_added ("lua" BINDING_WAS_ADDED) # bindings are required for tests
	if (BUILD_TESTING AND BINDING_WAS_ADDED)

		add_plugintest (
			lua MEMLEAK
			WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../../bindings/swig/lua/
			INSTALL_TEST_DATA)

	else ()
		message (WARNING "lua bindings are required for testing, test deactivated")
	endif ()
endif ()
