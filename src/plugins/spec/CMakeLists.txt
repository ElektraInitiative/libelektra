include (LibAddMacros)

add_plugin (spec
	SOURCES
		spec.h
		spec.c
		metafunctions.c
		metafunctions.h
	LINK_ELEKTRA
		elektra-ease
	   )

add_plugintest (spec)
