include (LibAddMacros)

add_plugin (
	shell TEST_README
	SOURCES shell.h shell.c COMPONENT libelektra${SO_VERSION})
