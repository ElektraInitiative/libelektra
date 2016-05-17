include (LibAddMacros)

add_plugin (rename
	SOURCES
		rename.h
		rename.c
	)

add_plugintest (rename rename.h rename.c LINK_PLUGIN)
