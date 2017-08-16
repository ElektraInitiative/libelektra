include (LibAddMacros)

add_plugin (size
	SOURCES
		size.h
		size.c
	LINK_ELEKTRA
		elektra-proposal
	ADD_TEST
	)
