include (LibAddPlugin)

add_plugin (
	base64
	SOURCES base64_functions.h base64_functions.c base64.h base64.c
	ADD_TEST TEST_README
	TEST_REQUIRED_PLUGINS toml COMPONENT libelektra${SO_VERSION})
