include (LibAddMacros)

add_plugin (hexcode
	SOURCES
		hexcode.h
		hexcode.c
	)

add_plugintest (hexcode hexcode.h hexcode.c LINK_PLUGIN)
