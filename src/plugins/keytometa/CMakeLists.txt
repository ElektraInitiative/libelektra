include (LibAddMacros)

add_plugin(keytometa
	SOURCES
		keytometa.h
		keytometa.c
	LINK_LIBRARIES
		elektra-proposal
	)

add_plugintest(keytometa)
