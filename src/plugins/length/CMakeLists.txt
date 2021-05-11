include (LibAddMacros)

add_plugin (
	length
	SOURCES length.h length.c test_length.h
	ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION}-experimental)
