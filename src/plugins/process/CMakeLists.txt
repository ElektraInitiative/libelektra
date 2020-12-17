include (LibAddMacros)

find_package (Pluginprocess)
plugin_check_if_included ("dump")

if (NOT_INCLUDED)
	message ("dump plugin not found (${NOT_INCLUDED}), excluding the process plugin")
	return ()
endif (NOT_INCLUDED)

if (PLUGINPROCESS_FOUND)
	add_plugin (
		process
		SOURCES process.h process.c
		ADD_TEST TEST_README
		TEST_REQUIRED_PLUGINS "dump"
		LINK_ELEKTRA elektra-pluginprocess elektra-invoke COMPONENT libelektra4-experimental)
else (PLUGINPROCESS_FOUND)
	remove_plugin (process "${PLUGINPROCESS_NOTFOUND_INFO}, pluginprocess library excluded, thus also excluding the process plugin")
endif (PLUGINPROCESS_FOUND)
