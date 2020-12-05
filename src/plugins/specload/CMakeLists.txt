include (LibAddMacros)

add_plugin (
	specload
	SOURCES specload.h specload.c
	LINK_ELEKTRA elektra-ease elektra-invoke
	TEST_README
	COMPONENT libelektra4)

if (DEPENDENCY_PHASE AND BUILD_SHARED)
	add_dependencies (elektra-specload elektra-quickdump)
endif ()

if (ADDTESTING_PHASE)
	add_executable (elektra-specload-testapp "${CMAKE_CURRENT_SOURCE_DIR}/testapp.c")
	target_link_elektra (elektra-specload-testapp elektra-core elektra-invoke)

	set (TESTAPP_PATH "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/elektra-specload-testapp")
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/config.c.in" "${CMAKE_CURRENT_BINARY_DIR}/config.c" @ONLY)

	set (TEST_SOURCES $<TARGET_OBJECTS:cframework> ${ARG_OBJECT_SOURCES})
	list (APPEND TEST_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/test_testapp.c")
	add_headers (TEST_SOURCES)
	add_testheaders (TEST_SOURCES)

	add_executable (test_elektra-specload-testapp ${TEST_SOURCES})
	target_link_elektra (test_elektra-specload-testapp elektra-core elektra-invoke)
	target_include_directories (test_elektra-specload-testapp PRIVATE "${CMAKE_SOURCE_DIR}/tests/cframework")
	target_include_directories (test_elektra-specload-testapp PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")

	add_test (
		NAME test_elektra-specload-testapp
		COMMAND "${CMAKE_BINARY_DIR}/bin/test_elektra-specload-testapp" "${CMAKE_CURRENT_SOURCE_DIR}"
		WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
	set_tests_properties (test_elektra-specload-testapp PROPERTIES TIMEOUT 30 ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")

	add_plugintest (
		specload INSTALL_TEST_DATA
		INCLUDE_DIRECTORIES ${CMAKE_CURRENT_BINARY_DIR}
		TIMEOUT 120)

	if (BUILD_SHARED)
		add_dependencies (elektra-specload-testapp elektra-quickdump)
		add_dependencies (test_elektra-specload-testapp elektra-specload-testapp)
		add_dependencies (testmod_specload elektra-specload-testapp)
	endif (BUILD_SHARED)
endif (ADDTESTING_PHASE)
