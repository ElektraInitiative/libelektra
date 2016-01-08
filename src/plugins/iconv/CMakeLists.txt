include (LibAddMacros)

add_plugin(iconv
	SOURCES
		conv.h
		iconv.c
	)

add_plugintest (iconv)
