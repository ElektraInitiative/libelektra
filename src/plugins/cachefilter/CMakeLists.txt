include (LibAddMacros)

add_plugin (cachefilter
	SOURCES
		cachefilter.h
		cachefilter.c
	)

add_plugintest (cachefilter)
