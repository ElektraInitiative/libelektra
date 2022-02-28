include (LibAddMacros)

add_plugin (
	process
	SOURCES process.h process.c
	LINK_ELEKTRA elektra-invoke COMPONENT libelektra${SO_VERSION}-experimental)

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/testapp.sh" "${CMAKE_BINARY_DIR}/bin/process-testapp.sh" COPYONLY)

install (
	PROGRAMS "${CMAKE_BINARY_DIR}/bin/process-testapp.sh"
	DESTINATION "${TARGET_TOOL_EXEC_FOLDER}"
	COMPONENT elektra-tests)

if (ADDTESTING_PHASE
    AND NOT ENABLE_ASAN
    AND NOT APPLE
    AND NOT WIN32)
	add_plugintest (process)
	if (ENABLE_KDB_TESTING AND "jna" IN_LIST ADDED_BINDINGS)
		add_msr_test_plugin ("process" ENVIRONMENT "BUILD_DIR=${CMAKE_BINARY_DIR}")
	endif ()
endif ()
