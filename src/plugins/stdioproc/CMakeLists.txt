include (LibAddMacros)

add_plugin (
	stdioproc
	SOURCES stdioproc.h stdioproc.c
	ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION}-experimental)
