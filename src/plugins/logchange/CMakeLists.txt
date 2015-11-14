include (LibAddMacros)

add_plugin(logchange
	SOURCES
		logchange.h
		logchange.c
	)

add_plugintest(logchange)
