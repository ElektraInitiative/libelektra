include (LibAddMacros)

add_plugin (
	validation
	SOURCES validation.h validation.c lookupre.c
	ADD_TEST COMPONENT libelektra${SO_VERSION})
