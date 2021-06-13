include (LibAddMacros)

add_plugin (
	blacklist
	SOURCES blacklist.h blacklist.c
	ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION}-extra)
