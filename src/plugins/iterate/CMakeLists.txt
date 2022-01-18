include (LibAddMacros)

add_plugin (
	iterate
	SOURCES iterate.h iterate.c
	ADD_TEST COMPONENT libelektra${SO_VERSION}-experimental)
