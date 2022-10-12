include (LibAddMacros)

add_plugin (
	modules
	SOURCES modules.h modules.c
	ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION})
