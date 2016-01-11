include (LibAddMacros)

add_plugin (spec
	SOURCES
		spec.h
		spec.c
    LINK_LIBRARIES
        elektra-ease
    )

add_plugintest (spec)
