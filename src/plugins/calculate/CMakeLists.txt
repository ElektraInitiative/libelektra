include (LibAddMacros)

add_plugin(calculate
	SOURCES
		calculate.h
		calculate.c
	)

add_plugintest(calculate)
