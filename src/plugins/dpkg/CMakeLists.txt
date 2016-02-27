include (LibAddMacros)

add_plugin (dpkg
	SOURCES
		dpkg.h
		dpkg.c
	)

add_plugintest (dpkg)
