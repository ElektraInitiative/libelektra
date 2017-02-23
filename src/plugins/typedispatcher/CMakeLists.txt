include (LibAddMacros)

add_plugin (typedispatcher
	SOURCES
		typedispatcher.h
		typedispatcher.c
		typehelper.h
		typehelper.c
	LINK_ELEKTRA
		elektra-proposal
		elektra-ease
	ADD_TEST
	)
