include (LibAddMacros)

add_plugin (validation
	SOURCES
		validation.h
		validation.c
		lookupre.c
	)

add_plugintest (validation lookupre.c)
