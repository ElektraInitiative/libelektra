add_plugin (
	type
	SOURCES type.h type.c types.h types.c
	LINK_ELEKTRA elektra-meta elektra-types
	ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION})
