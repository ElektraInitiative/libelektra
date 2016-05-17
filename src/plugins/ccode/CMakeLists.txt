include (LibAddMacros)

add_plugin (ccode
	SOURCES
		ccode.h
		ccode.c
	)

add_plugintest (ccode ccode.h ccode.c LINK_PLUGIN)
