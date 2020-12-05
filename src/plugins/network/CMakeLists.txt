include (LibAddMacros)

add_plugin (
	network
	SOURCES network.h network.c
	TEST_README
    COMPONENT libelektra4)

add_plugintest (network ../ipaddr/test_ipaddr.h)
