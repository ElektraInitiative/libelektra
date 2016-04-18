include (LibAddMacros)

add_plugin (keytometa
	SOURCES
		keytometa.h
		keytometa.c
	LINK_LIBRARIES
		elektra-proposal
		elektra-meta
	)

add_plugintest (keytometa)
