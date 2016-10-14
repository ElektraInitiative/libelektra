include (LibAddMacros)

add_plugin (enum
	SOURCES
		enum.h
		enum.c
	LINK_ELEKTRA
		elektra-meta
	ADD_TEST
	)
