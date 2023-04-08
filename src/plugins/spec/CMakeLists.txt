include (LibAddMacros)

add_plugin (
	spec
	SOURCES spec.h spec.c
	LINK_ELEKTRA elektra-ease elektra-meta elektra-globbing libm.so
	ADD_TEST COMPONENT libelektra${SO_VERSION})
