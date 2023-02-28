include (LibAddMacros)

add_plugin (
	backend
	SOURCES backend.h backend.c
	ADD_TEST COMPONENT libelektra${SO_VERSION})
