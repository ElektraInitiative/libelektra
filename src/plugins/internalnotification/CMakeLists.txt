include (LibAddMacros)

add_plugin (internalnotification
	SOURCES
		internalnotification.h
		internalnotification.c
		convert.c
	ADD_TEST
	LINK_ELEKTRA
		elektra-kdb
	)
