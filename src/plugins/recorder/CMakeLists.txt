include (LibAddMacros)

add_plugin (
	recorder
	SOURCES recorder.h recorder.c
	ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION}-experimental)
