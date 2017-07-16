include (LibAddMacros)

add_plugin (typedispatcher
	SOURCES
		typedispatcher.h
		typedispatcher.c
		typehelper.h
		helpers.c
		typereader.c
		typechecker.c
		pluginhelper.c
	LINK_ELEKTRA
		elektra-proposal
		elektra-ease
		elektra-kdb
	ADD_TEST
	)
