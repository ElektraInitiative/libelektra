include (LibAddMacros)

add_plugin (filecheck
	SOURCES
		filecheck.h
		filecheck.c
	)

add_plugintest (filecheck)
