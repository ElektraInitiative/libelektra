include (LibAddMacros)

add_plugin (cachefilter
	    SOURCES cachefilter.h cachefilter.c
	    TEST_README)

add_plugintest (cachefilter)
