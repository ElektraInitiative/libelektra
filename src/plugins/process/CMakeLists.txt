include (LibAddMacros)

find_package (Pluginprocess)

if (PLUGINPROCESS_FOUND)
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
else (PLUGINPROCESS_FOUND)
	message ("${PLUGINPROCESS_NOTFOUND_INFO}, excluding pluginprocess library, thus excluding the process plugin")
endif(PLUGINPROCESS_FOUND)
