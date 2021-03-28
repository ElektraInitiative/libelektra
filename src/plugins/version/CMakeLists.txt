include (LibAddMacros)

add_plugin (
	version
	SOURCES version.h version.c
	ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION})
