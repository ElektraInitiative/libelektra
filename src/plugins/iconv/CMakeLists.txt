include (LibAddMacros)

add_plugin(iconv
	SOURCES
		iconv.h
		iconv.c
	)

add_plugintest (iconv)
