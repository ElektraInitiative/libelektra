include (LibAddMacros)

set_source_files_properties (lineendings.c PROPERTIES COMPILE_FLAGS "-Wno-unused-result")

add_plugin (
	lineendings
	SOURCES lineendings.h lineendings.c
	ADD_TEST INSTALL_TEST_DATA COMPONENT libelektra${SO_VERSION}-extra)
