include (LibAddMacros)

add_plugin (
	network
	SOURCES network.h network.c
	TEST_README COMPONENT libelektra${SO_VERSION})

add_plugintest (network ../ipaddr/test_ipaddr.h)
