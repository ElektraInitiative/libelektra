include (LibAddMacros)

add_plugin (yaml
	SOURCES
		yaml.h
		yaml.c
	LINK_ELEKTRA
		elektra-ease
	ADD_TEST
	)
