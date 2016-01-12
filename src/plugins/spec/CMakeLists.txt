include (LibAddMacros)

add_plugin (spec
	SOURCES
		spec.h
		spec.c
        metafunctions.c
        metafunctions.h
    LINK_LIBRARIES
        elektra-ease
    )

add_plugintest (spec)
