include (LibAddMacros)

add_plugin (profile
	SOURCES
		profile.h
		profile.c
	LINK_ELEKTRA
		elektra-ease
	ADD_TEST
	)
