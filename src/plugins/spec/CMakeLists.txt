include (LibAddMacros)

add_plugin (spec
	SOURCES
		spec.h
		spec.c
	LINK_ELEKTRA
		elektra-ease
		elektra-meta
	ADD_TEST
	   )
