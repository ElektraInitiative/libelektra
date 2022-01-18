include (LibAddMacros)

add_plugin (
	mini
	SOURCES mini.h mini.c
	LINK_ELEKTRA elektra-ease elektra-utility
	ADD_TEST INSTALL_TEST_DATA TEST_README
	TEST_REQUIRED_PLUGINS ccode COMPONENT libelektra${SO_VERSION})
