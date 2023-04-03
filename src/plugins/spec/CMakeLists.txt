include (LibAddMacros)

add_plugin (
	spec
	SOURCES spec-new.h spec-new.c
	LINK_ELEKTRA elektra-ease elektra-meta elektra-globbing libm.so
	ADD_TEST COMPONENT libelektra${SO_VERSION})
