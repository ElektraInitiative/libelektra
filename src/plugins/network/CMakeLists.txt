include (LibAddMacros)

add_plugin (network
	SOURCES
		network.h
		network.c
	)

add_plugintest (network network.h network.c LINK_PLUGIN)
