include (LibAddMacros)

add_plugin (mini
	SOURCES
		mini.h
		mini.c
		values.h
	LINK_ELEKTRA
		elektra-utility
	ADD_TEST
	)
