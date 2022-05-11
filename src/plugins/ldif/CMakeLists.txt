include (LibAddMacros)

set_source_files_properties (ldif.c PROPERTIES COMPILE_FLAGS "-Wno-unused-result")

add_plugin (
	ldif
	SOURCES ldif.h ldif.c
	LINK_ELEKTRA elektra-ease elektra-kdb
	ADD_TEST INSTALL_TEST_DATA TEST_README
	TEST_REQUIRED_PLUGINS directoryvalue COMPONENT libelektra${SO_VERSION})
