include (LibAddMacros)

add_plugin (list
	SOURCES
		list.h
		list.c
	LINK_ELEKTRA
		elektra-kdb
	ADD_TEST
	)
