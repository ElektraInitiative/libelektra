include (LibAddMacros)

add_plugin (
	file
	SOURCES file.h file.c
	ADD_TEST INSTALL_TEST_DATA TEST_README COMPONENT libelektra${SO_VERSION}-experimental)
