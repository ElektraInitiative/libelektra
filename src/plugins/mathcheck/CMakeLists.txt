include (LibAddMacros)

add_plugin(mathcheck
	SOURCES
		mathcheck.h
		mathcheck.c
	)

add_plugintest(mathcheck)
