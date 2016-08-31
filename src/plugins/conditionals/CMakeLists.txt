include (LibAddMacros)

add_plugin (conditionals
	SOURCES
		conditionals.h
		conditionals.c
	LINK_ELEKTRA
		elektra-ease
		elektra-meta
		elektra-proposal
	ADD_TEST
	)
