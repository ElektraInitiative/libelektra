include (LibAddMacros)

add_plugin (conditionals
	SOURCES
		conditionals.h
		conditionals.c
	LINK_ELEKTRA
		elektra-meta
	ADD_TEST
	)
