include (LibAddMacros)

add_plugin (
	stdioproc
	SOURCES stdioproc.h stdioproc.c
	LINK_ELEKTRA elektra-invoke
	ADD_TEST COMPONENT libelektra${SO_VERSION}-experimental)

configure_file("${CMAKE_CURRENT_SOURCE_DIR}/testapp.sh" "${CMAKE_BINARY_DIR}/bin/stdioproc-testapp.sh" COPYONLY)

install (FILES "${CMAKE_BINARY_DIR}/bin/stdioproc-testapp.sh" DESTINATION "${TARGET_TOOL_EXEC_FOLDER}" COMPONENT elektra-tests)

if (ADDTESTING_PHASE AND ENABLE_KDB_TESTING AND "jna" IN_LIST ADDED_BINDINGS)
	add_msr_test_plugin ("stdioproc" ENVIRONMENT "BUILD_DIR=${CMAKE_BINARY_DIR}")
endif ()