include (LibAddMacros)

add_plugin (specload
	    SOURCES specload.h
		    specload.c
	    DEPENDS elektra-quickdump
	    LINK_ELEKTRA elektra-ease
			 elektra-invoke
	    TEST_README)

if (ADDTESTING_PHASE)
	add_executable (elektra-specload-testapp "${CMAKE_CURRENT_SOURCE_DIR}/testapp.c")
	target_link_elektra (elektra-specload-testapp elektra-core elektra-invoke)
	if (BUILD_SHARED)
		add_dependencies (elektra-specload-testapp elektra-quickdump)
	endif ()

	set (TESTAPP_PATH "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/elektra-specload-testapp")
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/config.c.in" "${CMAKE_CURRENT_BINARY_DIR}/config.c" @ONLY)

	add_plugintest (specload INSTALL_TEST_DATA INCLUDE_DIRECTORIES ${CMAKE_CURRENT_BINARY_DIR})
	add_dependencies (testmod_specload elektra-specload-testapp)
endif ()
