add_plugin (
	rename
	SOURCES rename.h rename.c
	ADD_TEST TEST_README
	TEST_REQUIRED_PLUGINS mini COMPONENT libelektra${SO_VERSION})
