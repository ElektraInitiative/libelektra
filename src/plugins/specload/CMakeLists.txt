include (LibAddMacros)

add_plugin (
	specload
	SOURCES specload.h specload.c
	LINK_ELEKTRA elektra-ease elektra-invoke
	TEST_README COMPONENT libelektra${SO_VERSION})

if (DEPENDENCY_PHASE AND BUILD_SHARED)
	add_dependencies (elektra-specload elektra-quickdump)
endif ()

if (ADDTESTING_PHASE)
	add_executable (elektra-specload-testapp "${CMAKE_CURRENT_SOURCE_DIR}/testapp.c")
	target_link_elektra (elektra-specload-testapp elektra-core elektra-invoke)

	set (TESTAPP_NAME "elektra-specload-testapp")
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/config.c.in" "${CMAKE_CURRENT_BINARY_DIR}/config.c" @ONLY)

	set (TEST_SOURCES $<TARGET_OBJECTS:cframework> ${ARG_OBJECT_SOURCES})
	list (APPEND TEST_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/test_testapp.c")

	add_executable (test_elektra-specload-testapp ${TEST_SOURCES})
	target_link_elektra (test_elektra-specload-testapp elektra-core elektra-invoke)
	target_include_directories (test_elektra-specload-testapp PRIVATE "${CMAKE_SOURCE_DIR}/tests/cframework")
	target_include_directories (test_elektra-specload-testapp PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")

	add_test (
		NAME test_elektra-specload-testapp
		COMMAND "${CMAKE_BINARY_DIR}/bin/test_elektra-specload-testapp" "${CMAKE_CURRENT_SOURCE_DIR}"
		WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
	set_tests_properties (test_elektra-specload-testapp PROPERTIES TIMEOUT 30)
	set_property (TEST test_elektra-specload-testapp PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib"
								  "KDB_TEST_BIN_DIR=${CMAKE_BINARY_DIR}/bin")

	add_plugintest (
		specload INSTALL_TEST_DATA
		INCLUDE_DIRECTORIES ${CMAKE_CURRENT_BINARY_DIR} EXTRA_EXECUTABLES elektra-specload-testapp
		TIMEOUT 120)

	if (BUILD_SHARED)
		add_dependencies (elektra-specload-testapp elektra-quickdump)
		if (NOT ENABLE_ASAN)
			add_dependencies (test_elektra-specload-testapp elektra-specload-testapp)
		endif (NOT ENABLE_ASAN)
		add_dependencies (testmod_specload elektra-specload-testapp)
	endif (BUILD_SHARED)
endif (ADDTESTING_PHASE)
