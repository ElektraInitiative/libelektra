include (LibAddMacros)

if (DEPENDENCY_PHASE)
	string (REPLACE "\"" "\\\"" C_FLAGS_ESCAPED ${CMAKE_C_FLAGS})
	string (REPLACE "\"" "\\\"" CXX_FLAGS_ESCAPED ${CMAKE_CXX_FLAGS})
	configure_file (constants.c.in ${CMAKE_CURRENT_BINARY_DIR}/constants.c)
endif ()

add_plugin (
	constants
	SOURCES ${CMAKE_CURRENT_BINARY_DIR}/constants.c
	TEST_README
	TEST_REQUIRED_PLUGINS noresolver COMPONENT libelektra${SO_VERSION})
