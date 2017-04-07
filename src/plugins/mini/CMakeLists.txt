include (LibAddMacros)

add_plugin (mini
	SOURCES
		mini.h
		mini.c
	LINK_ELEKTRA
		elektra-ease
		elektra-utility
	ADD_TEST
	)
