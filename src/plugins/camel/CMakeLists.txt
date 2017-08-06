include (LibAddMacros)

add_plugin (camel
	SOURCES
		camel.h
		camel.c
	LINK_ELEKTRA
		elektra-ease
	ADD_TEST
	)
