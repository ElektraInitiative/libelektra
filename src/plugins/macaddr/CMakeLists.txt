include (LibAddMacros)

add_plugin (
	macaddr
	SOURCES macaddr.h macaddr.c
	ADD_TEST TEST_README
	COMPONENT libelektra4-extra)
