include (LibAddMacros)

add_plugin (
	multifile
	SOURCES multifile.h multifile.c
	LINK_ELEKTRA elektra-kdb elektra-invoke
	TEST_README
	TEST_REQUIRED_PLUGINS toml COMPONENT libelektra${SO_VERSION})
