include (LibAddMacros)

add_plugin(simpleini
	SOURCES
		simpleini.h
		simpleini.c
	LINK_ELEKTRA
		elektra-plugin
	)

#add_plugintest (simpleini)
