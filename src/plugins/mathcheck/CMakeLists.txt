include (LibAddMacros)

add_plugin (mathcheck
	SOURCES
		mathcheck.h
		mathcheck.c
		floathelper.h
		floathelper.c
	)
add_plugintest (mathcheck)
