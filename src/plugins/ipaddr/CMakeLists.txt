include (LibAddMacros)

add_plugin (
	ipaddr
	SOURCES ipaddr.h ipaddr.c test_ipaddr.h
	ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION}-experimental)
