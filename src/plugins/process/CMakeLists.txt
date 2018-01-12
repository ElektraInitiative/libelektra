include (LibAddMacros)

add_plugin (process
	SOURCES
		process.h
		process.c
	ADD_TEST
	LINK_ELEKTRA
		elektra-pluginprocess
		elektra-invoke
	)
