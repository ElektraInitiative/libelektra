include (LibAddMacros)

add_plugin (keytometa
	SOURCES
		keytometa.h
		keytometa.c
	LINK_ELEKTRA
		elektra-proposal
		elektra-meta
	ADD_TEST
	)
