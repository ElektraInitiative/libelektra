include (LibAddMacros)

add_plugin (canonical
	SOURCES
		canonical.h
		canonical.c
	LINK_ELEKTRA
		elektra-meta
		elektra-utility
		elektra-ease
	ADD_TEST
	)
