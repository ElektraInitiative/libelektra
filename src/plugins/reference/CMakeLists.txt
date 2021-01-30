include (LibAddMacros)

add_plugin (
	reference
	SOURCES reference.h reference.c referencegraph.c referencegraph.h
	LINK_ELEKTRA elektra-ease elektra-globbing
	ADD_TEST TEST_README
	TEST_REQUIRED_PLUGINS dump sync COMPONENT libelektra${SO_VERSION}-experimental)
