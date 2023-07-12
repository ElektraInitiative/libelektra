include (LibAddMacros)

add_plugin (
	c
	SOURCES c.h c.c COMPONENT libelektra${SO_VERSION}
	ADD_TEST COMPONENT libelektra${SO_VERSION})
