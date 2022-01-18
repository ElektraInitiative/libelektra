include (LibAddMacros)

add_plugin (
	glob
	SOURCES glob.h glob.c
	ADD_TEST COMPONENT libelektra${SO_VERSION})
