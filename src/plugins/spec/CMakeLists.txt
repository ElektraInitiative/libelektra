include (LibAddMacros)

add_plugin (
	spec
	SOURCES matching.h matching.c arrayspec.h arrayspec.c spec.h spec.c
	LINK_ELEKTRA elektra-ease elektra-meta elektra-globbing libm.so
	ADD_TEST COMPONENT libelektra${SO_VERSION} ADD_TEST TEST_README)
