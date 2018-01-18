include (LibAddMacros)

add_plugin (process
	SOURCES
		process.h
		process.c
	ADD_TEST
	TEST_README
	TEST_REQUIRED_PLUGINS
		"dump"
	LINK_ELEKTRA
		elektra-pluginprocess
		elektra-invoke
	)
